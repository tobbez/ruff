use crate::lexer::TokenKind;
use ruff_text_size::TextRange;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum TriviaKind {
    NonLogicalNewline,
    Whitespace,
    LineContinuation,
    Comment,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Trivia {
    kind: TriviaKind,
    range: TextRange,
}

impl Trivia {
    pub(crate) fn new(kind: TriviaKind, range: TextRange) -> Self {
        Self { kind, range }
    }
}

impl TryFrom<TokenKind> for TriviaKind {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        let trivia_kind = match value {
            TokenKind::NonLogicalNewline => TriviaKind::NonLogicalNewline,
            TokenKind::Comment => TriviaKind::Comment,
            TokenKind::Whitespace => TriviaKind::Whitespace,
            TokenKind::LineContinuation => TriviaKind::LineContinuation,
            _ => return Err(()),
        };

        Ok(trivia_kind)
    }
}
