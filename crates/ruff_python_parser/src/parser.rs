use crate::lexer::{Lexer, Token, TokenKind};
use crate::syntax_factory::SyntaxFactory;
use crate::tokens::Tokens;
use crate::trivia::Trivia;
use crate::Mode;
use ruff_text_size::{TextRange, TextSize};

#[derive(Clone, Debug)]
pub struct Diagnostic {
    message: String,
    range: TextRange,
    hint: Option<String>,
}

pub(crate) struct Parser<'source> {
    last_end: TextSize,
    tokens: Tokens<'source>,
    factory: SyntaxFactory,
    diagnostics: Vec<Diagnostic>,
}

impl<'source> Parser<'source> {
    pub fn new(source_code: &'source str) -> Self {
        let tokens = Tokens::new(Lexer::new(source_code, TextSize::new(0)));
        Self {
            last_end: TextSize::new(0),
            tokens,
            factory: SyntaxFactory::default(),
            diagnostics: Vec::new(),
        }
    }

    pub fn finish(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    pub(crate) fn is_at(&self, kind: TokenKind) -> bool {
        self.current() == kind
    }

    pub(crate) fn current(&self) -> TokenKind {
        self.tokens.current().kind
    }

    pub(crate) fn current_range(&self) -> TextRange {
        self.tokens.range()
    }

    pub(crate) fn offset(&self) -> TextSize {
        self.tokens.start()
    }

    pub(crate) fn last_end(&self) -> TextSize {
        self.last_end
    }

    pub(crate) fn node_range(&self, start: TextSize) -> TextRange {
        TextRange::new(start, self.last_end)
    }

    pub(crate) fn peek(&mut self) -> TokenKind {
        self.tokens.peek().kind
    }

    pub(crate) fn factory(&mut self) -> &mut SyntaxFactory {
        &mut self.factory
    }

    #[inline]
    pub(crate) fn eat(&mut self, kind: TokenKind) -> bool {
        self.eat_token(kind).is_some()
    }

    pub(crate) fn eat_token(&mut self, kind: TokenKind) -> Option<Token<'source>> {
        debug_assert_ne!(kind, TokenKind::EndOfFile);
        if self.is_at(kind) {
            Some(self.do_bump())
        } else {
            None
        }
    }

    #[inline]
    pub(crate) fn expect(&mut self, kind: TokenKind) -> bool {
        self.expect_token(kind).is_some()
    }

    pub(crate) fn expect_logical_line_end(&mut self) {
        if self.eat(TokenKind::Newline) {
            // OK :)
        } else if self.is_at(TokenKind::EndOfFile) {
            // don't eat, but we're at the end of a logical line
        } else {
            self.diagnostics.push(Diagnostic {
                message: format!(
                    "Expected the end of a logical line but at {:?}",
                    self.current()
                ),
                range: self.current_range(),
                hint: None,
            });
        }
    }

    pub(crate) fn is_at_logical_line_end(&self) -> bool {
        matches!(self.current(), TokenKind::Newline | TokenKind::EndOfFile)
    }

    pub(crate) fn expect_token(&mut self, kind: TokenKind) -> Option<Token<'source>> {
        if self.is_at(kind) {
            Some(self.do_bump())
        } else {
            let message = if self.current() == TokenKind::EndOfFile {
                format!("Expected {kind:?} but instead the file ends")
            } else {
                format!("Expected {kind:?} but at {:?}.", self.current())
            };

            self.diagnostics.push(Diagnostic {
                message,
                range: self.current_range(),
                hint: None,
            });
            None
        }
    }

    pub(crate) fn bump(&mut self, kind: TokenKind) -> Token<'source> {
        assert_eq!(
            kind,
            self.current(),
            "expected {:?} but at {:?}",
            kind,
            self.current()
        );

        self.do_bump()
    }

    pub(crate) fn bump_remap(&mut self, kind: TokenKind) -> Token<'source> {
        assert_ne!(self.current(), TokenKind::EndOfFile);

        let mut current = self.do_bump();
        current.kind = kind;
        current
    }

    fn do_bump(&mut self) -> Token<'source> {
        self.last_end = self.current_range().end();
        self.tokens.bump()
    }
}

/// Captures the progress of the parser and allows to test if the parsing is still making progress
#[derive(Debug, Eq, Ord, PartialOrd, PartialEq, Hash, Default)]
pub(crate) struct ParserProgress(Option<TextSize>);

impl ParserProgress {
    /// Returns true if the current parser position is passed this position
    #[inline]
    pub(crate) fn has_progressed(&self, p: &Parser) -> bool {
        match self.0 {
            None => true,
            Some(pos) => pos < p.current_range().start(),
        }
    }

    /// Asserts that the parsing is still making progress.
    ///
    /// # Panics
    ///
    /// Panics if the parser is still at this position
    #[inline]
    pub(crate) fn assert_progressing(&mut self, p: &Parser) {
        assert!(
            self.has_progressed(p),
            "The parser is no longer progressing. Stuck at {:?}:{:?}",
            p.current(),
            p.current_range(),
        );

        self.0 = Some(p.current_range().start());
    }
}
