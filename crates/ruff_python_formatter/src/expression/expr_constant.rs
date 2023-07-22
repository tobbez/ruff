use ruff_text_size::{TextLen, TextRange};
use rustpython_parser::ast::{Constant, ExprConstant, Ranged};
use unicode_width::UnicodeWidthStr;

use ruff_formatter::{write, FormatContext, FormatOptions, FormatRuleWithOptions, IndentStyle};
use ruff_python_ast::node::AnyNodeRef;
use ruff_python_ast::str::is_implicit_concatenation;

use crate::expression::parentheses::{NeedsParentheses, OptionalParentheses};
use crate::expression::string::{FormatString, StringLayout, StringPrefix, StringQuotes};
use crate::prelude::*;
use crate::{not_yet_implemented_custom_text, verbatim_text, FormatNodeRule};

#[derive(Default)]
pub struct FormatExprConstant {
    layout: ExprConstantLayout,
}

#[derive(Copy, Clone, Debug, Default)]
pub enum ExprConstantLayout {
    #[default]
    Default,

    String(StringLayout),
}

impl FormatRuleWithOptions<ExprConstant, PyFormatContext<'_>> for FormatExprConstant {
    type Options = ExprConstantLayout;

    fn with_options(mut self, options: Self::Options) -> Self {
        self.layout = options;
        self
    }
}

impl FormatNodeRule<ExprConstant> for FormatExprConstant {
    fn fmt_fields(&self, item: &ExprConstant, f: &mut PyFormatter) -> FormatResult<()> {
        let ExprConstant {
            range: _,
            value,
            kind: _,
        } = item;

        match value {
            Constant::Ellipsis => text("...").fmt(f),
            Constant::None => text("None").fmt(f),
            Constant::Bool(value) => match value {
                true => text("True").fmt(f),
                false => text("False").fmt(f),
            },
            Constant::Int(_) | Constant::Float(_) | Constant::Complex { .. } => {
                write!(f, [verbatim_text(item)])
            }
            Constant::Str(_) => {
                let string_layout = match self.layout {
                    ExprConstantLayout::Default => StringLayout::Default,
                    ExprConstantLayout::String(layout) => layout,
                };
                FormatString::new(item).with_layout(string_layout).fmt(f)
            }
            Constant::Bytes(_) => {
                not_yet_implemented_custom_text(r#"b"NOT_YET_IMPLEMENTED_BYTE_STRING""#).fmt(f)
            }
        }
    }

    fn fmt_dangling_comments(
        &self,
        _node: &ExprConstant,
        _f: &mut PyFormatter,
    ) -> FormatResult<()> {
        Ok(())
    }
}

impl NeedsParentheses for ExprConstant {
    fn needs_parentheses(
        &self,
        parent: AnyNodeRef,
        context: &PyFormatContext,
    ) -> OptionalParentheses {
        if self.value.is_str() {
            let contents = context.locator().slice(self.range());
            // Don't wrap "pointless" strings
            if parent.is_stmt_expr() {
                OptionalParentheses::Never
            } else if is_multiline_string(self, context.source()) {
                OptionalParentheses::Never
            } else if is_implicit_concatenation(contents) {
                OptionalParentheses::Multiline
            } else if parent.is_expr_call() || parent.is_expr_attribute() {
                OptionalParentheses::Never
            } else {
                // let line_width = usize::from(context.options().line_width().value());
                // let tab_width = match context.options().indent_style() {
                //     IndentStyle::Tab => 4,
                //     IndentStyle::Space(spaces) => spaces as usize,
                // };
                // // This is no perfect science, just a heuristic. But more than 8 levels of indent should be rare
                // // and it allows us to rule out most best fitting usages.
                // let length_threshold = line_width.saturating_sub(tab_width * 8);
                //
                // // We don't care about unicode here, it's just a heuristic
                // if contents.len() > length_threshold {
                //     OptionalParentheses::NonSplitableMultiline
                // } else {
                //     OptionalParentheses::Multiline
                // }
                OptionalParentheses::NonSplitableMultiline
            }
        } else {
            OptionalParentheses::Never
        }
    }
}

pub(super) fn is_multiline_string(constant: &ExprConstant, source: &str) -> bool {
    if constant.value.is_str() {
        let contents = &source[constant.range()];
        let prefix = StringPrefix::parse(contents);
        let quotes =
            StringQuotes::parse(&contents[TextRange::new(prefix.text_len(), contents.text_len())]);

        quotes.map_or(false, StringQuotes::is_triple) && contents.contains(['\n', '\r'])
    } else {
        false
    }
}
