use crate::builders::parenthesize_if_expands;
use ruff_formatter::{format_args, write, FormatError};
use ruff_python_ast::{AnyNodeRef, Expr, ExpressionRef, StmtAssign};

use crate::comments::{trailing_comments, SourceComment, SuppressionKind};
use crate::context::{NodeLevel, WithNodeLevel};
use crate::expression::parentheses::{
    is_expression_parenthesized, NeedsParentheses, OptionalParentheses, Parentheses, Parenthesize,
};
use crate::expression::{has_own_parentheses, maybe_parenthesize_expression};
use crate::prelude::*;
use crate::preview::is_prefer_splitting_right_hand_side_of_assignments_enabled;
use crate::statement::trailing_semicolon;

#[derive(Default)]
pub struct FormatStmtAssign;

impl FormatNodeRule<StmtAssign> for FormatStmtAssign {
    fn fmt_fields(&self, item: &StmtAssign, f: &mut PyFormatter) -> FormatResult<()> {
        let StmtAssign {
            range: _,
            targets,
            value,
        } = item;

        let (first, rest) = targets.split_first().ok_or(FormatError::syntax_error(
            "Expected at least on assignment target",
        ))?;

        write!(f, [first.format(), space(), token("="), space()])?;

        if is_prefer_splitting_right_hand_side_of_assignments_enabled(f.context()) {
            for target in rest {
                if has_own_parentheses(target, f.context()).is_some()
                    && !f.context().comments().has_leading(target)
                {
                    target.format().with_options(Parentheses::Never).fmt(f)?;
                } else {
                    parenthesize_if_expands(&target.format().with_options(Parentheses::Never))
                        .fmt(f)?;
                }
                write!(f, [space(), token("="), space()])?;
            }
        } else {
            FormatTargets { targets: rest }.fmt(f)?;
        }

        FormatStatementsLastExpression::new(value, item).fmt(f)?;

        if f.options().source_type().is_ipynb()
            && f.context().node_level().is_last_top_level_statement()
            && trailing_semicolon(item.into(), f.context().source()).is_some()
            && matches!(targets.as_slice(), [Expr::Name(_)])
        {
            token(";").fmt(f)?;
        }

        Ok(())
    }

    fn is_suppressed(
        &self,
        trailing_comments: &[SourceComment],
        context: &PyFormatContext,
    ) -> bool {
        SuppressionKind::has_skip_comment(trailing_comments, context.source())
    }
}

#[derive(Debug)]
struct FormatTargets<'a> {
    targets: &'a [Expr],
}

impl Format<PyFormatContext<'_>> for FormatTargets<'_> {
    fn fmt(&self, f: &mut PyFormatter) -> FormatResult<()> {
        if let Some((first, rest)) = self.targets.split_first() {
            let comments = f.context().comments();

            let parenthesize = if comments.has_leading(first) {
                ParenthesizeTarget::Always
            } else if has_own_parentheses(first, f.context()).is_some() {
                ParenthesizeTarget::Never
            } else {
                ParenthesizeTarget::IfBreaks
            };

            let group_id = if parenthesize == ParenthesizeTarget::Never {
                Some(f.group_id("assignment_parentheses"))
            } else {
                None
            };

            let format_first = format_with(|f: &mut PyFormatter| {
                let mut f = WithNodeLevel::new(NodeLevel::Expression(group_id), f);
                match parenthesize {
                    ParenthesizeTarget::Always => {
                        write!(f, [first.format().with_options(Parentheses::Always)])
                    }
                    ParenthesizeTarget::Never => {
                        write!(f, [first.format().with_options(Parentheses::Never)])
                    }
                    ParenthesizeTarget::IfBreaks => {
                        write!(
                            f,
                            [
                                if_group_breaks(&token("(")),
                                soft_block_indent(&first.format().with_options(Parentheses::Never)),
                                if_group_breaks(&token(")"))
                            ]
                        )
                    }
                }
            });

            write!(
                f,
                [group(&format_args![
                    format_first,
                    space(),
                    token("="),
                    space(),
                    FormatTargets { targets: rest }
                ])
                .with_group_id(group_id)]
            )
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParenthesizeTarget {
    Always,
    Never,
    IfBreaks,
}

/// Formats the last expression in statements that start with a keyword (like `return`) or after an operator (assignments).
///
/// It avoids parenthesizing unsplittable values (like `None`, `True`, `False`, Names, a subset of strings) just to make
/// the trailing comment fit and inlines a trailing comment if the value itself exceeds the configured line width:
///
/// The implementation formats the statement's and value's trailing end of line comments:
/// * after the expression if the expression needs no parentheses (necessary or the `expand_parent` makes the group never fit).
/// * inside the parentheses if the expression exceeds the line-width.
///
/// ```python
/// a = loooooooooooooooooooooooooooong # with_comment
/// b = (
///     short # with_comment
/// )
/// ```
///
/// Which gets formatted to:
///
/// ```python
/// # formatted
/// a = (
///     loooooooooooooooooooooooooooong # with comment
/// )
/// b = short # with comment
/// ```
///
/// The long name gets parenthesized because it exceeds the configured line width and the trailing comma of the
/// statement gets formatted inside (instead of outside) the parentheses.
///
/// The `short` name gets unparenthesized because it fits into the configured line length, regardless of whether
/// the comment exceeds the line width or not.
///
/// This logic isn't implemented in [`place_comment`] by associating trailing statement comments to the expression because
/// doing so breaks the suite empty lines formatting that relies on trailing comments to be stored on the statement.
pub(super) struct FormatStatementsLastExpression<'a> {
    expression: &'a Expr,
    parent: AnyNodeRef<'a>,
}

impl<'a> FormatStatementsLastExpression<'a> {
    pub(super) fn new<P: Into<AnyNodeRef<'a>>>(expression: &'a Expr, parent: P) -> Self {
        Self {
            expression,
            parent: parent.into(),
        }
    }
}

impl Format<PyFormatContext<'_>> for FormatStatementsLastExpression<'_> {
    fn fmt(&self, f: &mut Formatter<PyFormatContext<'_>>) -> FormatResult<()> {
        let can_inline_comment = match self.expression {
            Expr::Name(_)
            | Expr::NoneLiteral(_)
            | Expr::NumberLiteral(_)
            | Expr::BooleanLiteral(_) => true,
            Expr::StringLiteral(string) => {
                string.needs_parentheses(self.parent, f.context()) == OptionalParentheses::BestFit
            }
            Expr::BytesLiteral(bytes) => {
                bytes.needs_parentheses(self.parent, f.context()) == OptionalParentheses::BestFit
            }
            Expr::FString(fstring) => {
                fstring.needs_parentheses(self.parent, f.context()) == OptionalParentheses::BestFit
            }
            _ => false,
        };

        // BestFitParenthesize with mode group doesn't really work, because groups to the left avoid breaking the content
        // assuming that the parenthesized content will break. However, the parenthesized content may then end up
        // too-long, so that it doesn't get parenthesized. The result is that we didn't split a line at positions where we could,
        // resulting in much longer lines. Meaning, the `mode`: `Group` inherently doesn't work

        if !can_inline_comment {
            return maybe_parenthesize_expression(
                self.expression,
                self.parent,
                Parenthesize::IfBreaks,
            )
            .fmt(f);
        }

        let comments = f.context().comments().clone();
        let expression_comments = comments.leading_dangling_trailing(self.expression);

        if expression_comments.has_leading() {
            // Preserve the parentheses if the expression has any leading comments,
            // same as `maybe_parenthesize_expression`
            return self
                .expression
                .format()
                .with_options(Parentheses::Always)
                .fmt(f);
        }

        let statement_trailing_comments = comments.trailing(self.parent);
        let after_end_of_line = statement_trailing_comments
            .partition_point(|comment| comment.line_position().is_end_of_line());
        let (stmt_inline_comments, _) = statement_trailing_comments.split_at(after_end_of_line);

        let after_end_of_line = expression_comments
            .trailing
            .partition_point(|comment| comment.line_position().is_end_of_line());

        let (expression_inline_comments, expression_trailing_comments) =
            expression_comments.trailing.split_at(after_end_of_line);

        if expression_trailing_comments.is_empty() {
            let inline_comments = OptionalParenthesesInlinedComments {
                expression: expression_inline_comments,
                statement: stmt_inline_comments,
            };

            let group_id = f.group_id("optional_parentheses");

            // Black always parenthesizes if the right side is splittable, either because it has multiple targets OR
            // the expression itself can be split (not a name or attribute chain with names only).
            let best_fit_layout =
                !is_prefer_splitting_right_hand_side_of_assignments_enabled(f.context())
                    || !is_assignment_with_splittable_targets(self.parent, f.context());

            if best_fit_layout {
                let f = &mut WithNodeLevel::new(NodeLevel::Expression(Some(group_id)), f);

                best_fit_parenthesize(&format_with(|f| {
                    inline_comments.mark_formatted();

                    self.expression
                        .format()
                        .with_options(Parentheses::Never)
                        .fmt(f)?;

                    if !inline_comments.is_empty() {
                        // If the expressions exceeds the line width, format the comments in the parentheses
                        if_group_breaks(&inline_comments).fmt(f)?;
                    }

                    Ok(())
                }))
                .with_group_id(Some(group_id))
                .fmt(f)?;
            } else {
                inline_comments.mark_formatted();

                group(&format_args![
                    if_group_breaks(&token("(")),
                    indent(&format_args![
                        soft_line_break(),
                        self.expression.format().with_options(Parentheses::Never)
                    ])
                ])
                .with_group_id(Some(group_id))
                .fmt(f)?;

                // It's necessary to format the comments outside the group or they would
                // force expand the group. This is a bit sketchy, because it no longer measures whether the
                // closing `)` fits. However, the closing `)` should always fit if the opening `)` did.
                if !inline_comments.is_empty() {
                    // If the expressions exceeds the line width, format the comments in the parentheses
                    if_group_breaks(&inline_comments)
                        .with_group_id(Some(group_id))
                        .fmt(f)?;
                };

                if_group_breaks(&format_args![soft_line_break(), token(")")])
                    .with_group_id(Some(group_id))
                    .fmt(f)?;
            }

            if !inline_comments.is_empty() {
                // If the line fits into the line width, format the comments after the parenthesized expression
                if_group_fits_on_line(&inline_comments)
                    .with_group_id(Some(group_id))
                    .fmt(f)?;
            }

            Ok(())
        } else {
            self.expression
                .format()
                .with_options(Parentheses::Always)
                .fmt(f)
        }
    }
}

#[derive(Debug, Default)]
struct OptionalParenthesesInlinedComments<'a> {
    expression: &'a [SourceComment],
    statement: &'a [SourceComment],
}

impl<'a> OptionalParenthesesInlinedComments<'a> {
    fn is_empty(&self) -> bool {
        self.expression.is_empty() && self.statement.is_empty()
    }

    fn iter_comments(&self) -> impl Iterator<Item = &'a SourceComment> {
        self.expression.iter().chain(self.statement)
    }

    fn mark_formatted(&self) {
        for comment in self.expression {
            comment.mark_formatted();
        }
    }
}

impl Format<PyFormatContext<'_>> for OptionalParenthesesInlinedComments<'_> {
    fn fmt(&self, f: &mut Formatter<PyFormatContext<'_>>) -> FormatResult<()> {
        for comment in self.iter_comments() {
            comment.mark_unformatted();
        }

        write!(
            f,
            [
                trailing_comments(self.expression),
                trailing_comments(self.statement)
            ]
        )
    }
}

/// Returns `true` if the passed assignment like node can be split in case the assignment statement doesn't fit on a single line.
///
/// Knowing whether the assignment can split is required because Ruff uses the [`BestFit`] layout for
/// non-splittable values like strings, numbers, names, etc. but only if the assignment can't be split as well.
/// The motivation for the besst fit layout is to avoid unnecessary parentheses:
///
/// ```python
/// unsplittable = very_long_value
/// ```
///
/// is preferred over
///
/// ```python
/// unsplittable = (
///     very_long_value
/// )
/// ```
///
/// if the `very_long_value` doesn't fit even when parenthesizing it.
///
/// Ideally, the best fit layout is applied regardless of whether the target(s) can split and changing
/// [`BestFitParenthesize`] to have a group like semantic where the right breaks before the left would be sufficient.
/// However, it is possible to run into the situation where the target decides **not** to split
/// because there's sufficient space to print the opening parentheses that is followed by a newline (break right before left).
/// However, [`BestFitParenthesize`] may now decide not to parenthesize because the value doesn't fit even when parenthesized,
/// in which case it would have been preferred to, at least, split the targets to make some more space.
///
/// Changing the `target` group to continue measuring to ensure that the `value` fits would be hard to add to our Printer
/// that generally only tests if the content up to the next soft or hard line break fits.
///
/// That's why we use this check for now. Using this check also aligns with Black's behavior to always
/// parenthesize values if they exceed the line-width when the target(s) is/are splittable.
pub(crate) fn is_assignment_with_splittable_targets(
    assignment: AnyNodeRef,
    context: &PyFormatContext,
) -> bool {
    match assignment {
        AnyNodeRef::StmtAssign(StmtAssign { targets, .. }) => {
            if let [only] = targets.as_slice() {
                is_splittable_target_or_annotation(only)
            } else {
                true
            }
        }
        AnyNodeRef::StmtTypeAlias(alias) => {
            alias.type_params.is_some() || is_splittable_target_or_annotation(&alias.name)
        }
        AnyNodeRef::StmtAnnAssign(assign) => {
            is_splittable_target_or_annotation(&assign.target)
                || is_splittable_target_or_annotation(&assign.annotation)
                // TODO(micha): Remove when implementing the type annotation split preview style
                || is_expression_parenthesized(
                    ExpressionRef::from(&assign.annotation),
                    context.comments().ranges(),
                    context.source(),
                )
        }
        AnyNodeRef::StmtAugAssign(assign) => is_splittable_target_or_annotation(&assign.target),
        _ => false,
    }
}

// TODO it seems Black uses the best fit layout also when it is known that the target will split either
// because of a comment OR a magic trailing comma.
const fn is_splittable_target_or_annotation(target: &Expr) -> bool {
    match target {
        Expr::Name(_) => false,
        Expr::Attribute(attribute) => is_splittable_target_or_annotation(&attribute.value),
        Expr::StringLiteral(literal) => literal.value.is_implicit_concatenated(),
        _ => true,
    }
}
