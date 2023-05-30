use crate::expression::parse_expression;
use crate::lexer::TokenKind;
use crate::parser::{Parser, ParserProgress};
use crate::Parse;
use ruff_text_size::TextRange;
use rustpython_ast::{ExprContext, Identifier, Stmt, StmtWhile};
use std::os::linux::raw::stat;

pub(super) fn parse_statements(p: &mut Parser) -> Vec<Stmt> {
    let mut statements = Vec::new();
    let mut progress = ParserProgress::default();

    dbg!(p.current());
    while !p.is_at(TokenKind::EndOfFile) && !p.is_at(TokenKind::Dedent) {
        progress.assert_progressing(p);

        statements.push(parse_statement(p));
    }

    statements
}

pub(super) fn parse_statement(p: &mut Parser) -> Stmt {
    let start = p.offset();
    match dbg!(p.current()) {
        TokenKind::Break => {
            p.bump(TokenKind::Break);
            let range = p.node_range(start);
            p.factory().new_break_statement(range).into()
        }
        TokenKind::Continue => {
            p.bump(TokenKind::Break);
            let range = p.node_range(start);
            p.factory().new_continue_statement(range).into()
        }
        TokenKind::Pass => {
            p.bump(TokenKind::Break);
            let range = p.node_range(start);
            p.factory().new_break_statement(range).into()
        }
        TokenKind::Return => {
            p.bump(TokenKind::Return);
            let value = parse_expression(p);
            let range = p.node_range(start);
            p.factory().new_return_statement(range, value).into()
        }
        TokenKind::While => parse_while_statement(p).into(),
        _ => {
            if let Some(expression) = parse_expression(p) {
                let range = p.node_range(start);
                p.factory()
                    .new_expression_statement(range, expression)
                    .into()
            } else {
                todo!()
            }
        }
    }
}

fn parse_while_statement(p: &mut Parser) -> StmtWhile {
    let start = p.offset();
    p.expect(TokenKind::While);

    let expresssion = if let Some(expression) = parse_expression(p) {
        expression
    } else {
        // TODO create diagnostic and maybe synthesize node?
        let range = TextRange::empty(p.offset());
        p.factory()
            .new_name_expression(range, Identifier::new(String::new()), ExprContext::Load)
            .into()
    };

    p.expect(TokenKind::Colon);

    let body = parse_body(p);

    let alternate = if p.eat(TokenKind::Else) {
        p.expect(TokenKind::Colon);
        parse_body(p)
    } else {
        Vec::new()
    };

    let range = p.node_range(start);
    p.factory()
        .new_while_statement(range, expresssion, body, alternate)
}

fn parse_body(p: &mut Parser) -> Vec<Stmt> {
    if p.eat(TokenKind::Newline) {
        p.expect(TokenKind::Indent);
        let mut statements = Vec::new();
        let mut progress = ParserProgress::default();

        while !p.is_at(TokenKind::Dedent) && !p.is_at(TokenKind::EndOfFile) {
            progress.assert_progressing(p);

            statements.push(parse_statement(p));

            while p.eat(TokenKind::Semi) {
                // TODO split parse_simple_statement out or handle inside of parse_statement?
                statements.push(parse_statement(p));
            }

            // TODO not all statements require a newline at the end?
            p.expect_logical_line_end();
        }

        p.expect(TokenKind::Dedent);

        statements
    } else {
        let mut statements = vec![parse_statement(p)];

        while p.eat(TokenKind::Semi) {
            statements.push(parse_statement(p));
        }

        statements
    }
}
