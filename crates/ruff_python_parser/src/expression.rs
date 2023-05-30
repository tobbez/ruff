use crate::lexer::TokenKind;
use crate::parser::{Parser, ParserProgress};
use ruff_text_size::TextRange;
use rustpython_ast::{Expr, ExprContext, Identifier};

pub(super) fn parse_expression(p: &mut Parser) -> Option<Expr> {
    let start = p.offset();
    let expression: Expr = match p.current() {
        TokenKind::Identifier => {
            let identifier_token = p.bump(TokenKind::Identifier);
            let identifier = Identifier::new(
                identifier_token
                    .value
                    .expect("Expected identifier for `Identifier` token"),
            );

            let range = p.node_range(start);
            p.factory()
                .new_name_expression(range, identifier, ExprContext::Load)
                .into()
        }

        TokenKind::String => {
            let string_token = p.bump(TokenKind::String);
            let string_value = string_token
                .value
                .expect("Expected string for `String` token");
            let range = p.node_range(start);
            p.factory()
                .new_string_literal_expression(range, string_value)
                .into()
        }

        _ => return None,
    };

    Some(expression)
}
