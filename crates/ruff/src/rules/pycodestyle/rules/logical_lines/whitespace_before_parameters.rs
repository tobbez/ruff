use rustpython_parser::ast::Location;
use rustpython_parser::Tok;

use ruff_diagnostics::{AlwaysAutofixableViolation, Diagnostic, Edit};
use ruff_macros::{derive_message_formats, violation};
use ruff_python_ast::types::Range;

use super::LogicalLineTokens;

#[violation]
pub struct WhitespaceBeforeParameters {
    pub bracket: String,
}

impl AlwaysAutofixableViolation for WhitespaceBeforeParameters {
    #[derive_message_formats]
    fn message(&self) -> String {
        let WhitespaceBeforeParameters { bracket } = self;
        format!("Whitespace before {bracket}")
    }

    fn autofix_title(&self) -> String {
        let WhitespaceBeforeParameters { bracket } = self;
        format!("Removed whitespace before {bracket}")
    }
}

/// E211
pub(crate) fn whitespace_before_parameters(
    tokens: &LogicalLineTokens,
    autofix: bool,
) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    let previous = tokens.first().unwrap();

    let mut pre_pre_kind: Option<&Tok> = None;
    let mut prev_token = previous.kind();
    let mut prev_end = previous.end();

    for token in tokens {
        let kind = token.kind();

        if matches!(kind, Tok::Lpar | Tok::Lsqb)
            && token.start() != prev_end
            && matches!(
                prev_token,
                Tok::Name { .. } | Tok::Rpar | Tok::Rsqb | Tok::Rbrace
            )
            && (pre_pre_kind != Some(&Tok::Class))
        {
            let start = Location::new(prev_end.row(), prev_end.column());
            let end = token.end();
            let end = Location::new(end.row(), end.column() - 1);

            let kind: WhitespaceBeforeParameters = WhitespaceBeforeParameters {
                bracket: kind.to_string(),
            };

            let mut diagnostic = Diagnostic::new(kind, Range::new(start, end));

            if autofix {
                diagnostic.set_fix(Edit::deletion(start, end));
            }
            diagnostics.push(diagnostic);
        }
        pre_pre_kind = Some(prev_token);
        prev_token = kind;
        prev_end = token.end();
    }
    diagnostics
}