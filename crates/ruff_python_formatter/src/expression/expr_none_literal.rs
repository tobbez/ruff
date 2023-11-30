use ruff_python_ast::AnyNodeRef;
use ruff_python_ast::ExprNoneLiteral;

use crate::expression::parentheses::{NeedsParentheses, OptionalParentheses};
use crate::prelude::*;
use crate::preview::is_prefer_splitting_right_hand_side_of_assignments_enabled;
use crate::statement::stmt_assign::is_assignment_with_splittable_targets;

#[derive(Default)]
pub struct FormatExprNoneLiteral;

impl FormatNodeRule<ExprNoneLiteral> for FormatExprNoneLiteral {
    fn fmt_fields(&self, _item: &ExprNoneLiteral, f: &mut PyFormatter) -> FormatResult<()> {
        token("None").fmt(f)
    }
}

impl NeedsParentheses for ExprNoneLiteral {
    fn needs_parentheses(
        &self,
        parent: AnyNodeRef,
        context: &PyFormatContext,
    ) -> OptionalParentheses {
        if is_prefer_splitting_right_hand_side_of_assignments_enabled(context)
            && is_assignment_with_splittable_targets(parent, context)
        {
            OptionalParentheses::Multiline
        } else {
            OptionalParentheses::BestFit
        }
    }
}
