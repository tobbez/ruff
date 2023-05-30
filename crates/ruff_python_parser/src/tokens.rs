use crate::lexer::{Lexer, Token, TokenKind};
use crate::trivia::{Trivia, TriviaKind};
use ruff_text_size::{TextRange, TextSize};

#[derive(Clone, Debug)]
pub struct Tokens<'source> {
    offset: TextSize,
    lexer: Lexer<'source>,
    current: Token<'source>,
    next: Option<Token<'source>>,
    trivia: Vec<Trivia>,
}

impl<'source> Tokens<'source> {
    pub fn new(mut lexer: Lexer<'source>) -> Self {
        let mut tokens = Self {
            offset: TextSize::new(0),
            current: Token::new(TokenKind::EndOfFile, TextSize::new(0)),
            lexer,
            next: None,
            trivia: Vec::new(),
        };

        tokens.bump();
        tokens
    }

    pub fn current(&self) -> &Token<'source> {
        &self.current
    }

    pub fn peek(&mut self) -> &Token<'source> {
        if self.next.is_none() {
            self.next = Some(self.next_non_trivia_token());
        }

        // SAFETY: We assign `self.next` to `Some` if it was `None` above.
        #[allow(unsafe_code)]
        unsafe {
            self.next.as_ref().unwrap_unchecked()
        }
    }

    pub fn range(&self) -> TextRange {
        TextRange::at(self.start(), self.current.length)
    }

    pub fn start(&self) -> TextSize {
        self.offset - self.current.length
    }

    pub fn end(&self) -> TextSize {
        self.offset
    }

    pub fn next_range(&mut self) -> TextRange {
        let offset = self.offset;
        let next = self.peek();

        TextRange::at(offset, next.length)
    }

    pub fn bump(&mut self) -> Token<'source> {
        let next = if let Some(next) = self.next.take() {
            next
        } else {
            self.next_non_trivia_token()
        };

        let current = std::mem::replace(&mut self.current, next);

        self.offset += self.current.length;

        current
    }

    fn next_non_trivia_token(&mut self) -> Token<'source> {
        loop {
            let next = self.lexer.next_token();

            dbg!(&next);

            // Skip trivia tokens
            if let Ok(trivia_kind) = TriviaKind::try_from(next.kind) {
                self.trivia.push(Trivia::new(
                    trivia_kind,
                    TextRange::at(self.offset, next.length),
                ));
                self.offset += next.length;
            } else {
                break next;
            }
        }
    }
}
