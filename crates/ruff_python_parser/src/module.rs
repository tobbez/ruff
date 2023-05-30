use crate::lexer::TokenKind;

use crate::parser::{Parser, ParserProgress};
use crate::statements::parse_statements;
use ruff_text_size::TextRange;
use rustpython_ast::ModModule;

pub(super) fn parse_module(p: &mut Parser) -> ModModule {
    let start = p.offset();
    let statements = parse_statements(p);
    let range = TextRange::new(start, p.last_end());
    p.factory().new_module(range, statements, Vec::new())
}
