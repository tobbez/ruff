use ruff_python_ast::AnyNodeRef;
use ruff_python_ast::ExprBooleanLiteral;

use crate::expression::parentheses::{NeedsParentheses, OptionalParentheses};
use crate::prelude::*;
use crate::preview::is_prefer_splitting_right_hand_side_of_assignments_enabled;
use crate::statement::stmt_assign::is_assignment_with_splittable_targets;

#[derive(Default)]
pub struct FormatExprBooleanLiteral;

impl FormatNodeRule<ExprBooleanLiteral> for FormatExprBooleanLiteral {
    fn fmt_fields(&self, item: &ExprBooleanLiteral, f: &mut PyFormatter) -> FormatResult<()> {
        if item.value {
            token("True").fmt(f)
        } else {
            token("False").fmt(f)
        }
    }
}

impl NeedsParentheses for ExprBooleanLiteral {
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
