use std::iter::Peekable;

use crate::bonobo::lexer::{EOF_TOKEN, Token, TokenId};

use super::error::ParseError;

#[derive(Debug)]
pub struct ParseContext<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
    errors: Vec<ParseError>,
    has_fatal_error: bool,
}

pub fn is_token_symbol(a: TokenId) -> impl Fn(&TokenId) -> bool {
    move |b| *b == a
}

impl<I: Iterator<Item = Token>> ParseContext<I> {
    pub fn new(tokens: I) -> Self {
        ParseContext {
            tokens: tokens.peekable(),
            errors: vec![],
            has_fatal_error: false,
        }
    }

    pub fn advance(&mut self) -> Token {
        match self.tokens.next() {
            Some(token) => token,
            _ => EOF_TOKEN,
        }
    }

    pub fn peek(&mut self) -> &Token {
        match self.tokens.peek() {
            Some(token) => token,
            _ => &EOF_TOKEN,
        }
    }

    pub fn expect<F>(&mut self, matcher: F) -> bool
    where
        F: Fn(&TokenId) -> bool,
    {
        matches!(self.tokens.peek(), Some(Token { id, .. }) if matcher(id))
    }

    /// Expect the next token to match the given matching function
    /// and advance the tokenstream if it matches
    ///
    /// Will record the error if the encountered token does not match
    /// the expected token and returns false
    pub fn advance_required<F>(&mut self, matcher: F) -> bool
    where
        F: Fn(&TokenId) -> bool,
    {
        if self.expect(&matcher) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Will advance the token stream only if the expected token is found
    /// Returns whether expected token is found
    pub fn advance_optional<F>(&mut self, matcher: F) -> bool
    where
        F: Fn(&TokenId) -> bool,
    {
        let result = self.expect(&matcher);
        if result {
            let _ = self.advance();
        }
        result
    }

    pub fn advance_required_symbol_logged(&mut self, id: TokenId) -> bool {
        let result = self.advance_required(is_token_symbol(id));

        if !result {
            let s = ParseError::UnexpectedToken(self.peek().clone());
            self.log_error(s);
        }

        result
    }

    pub fn has_fatal(&self) -> bool {
        self.has_fatal_error
    }

    pub fn log_error(&mut self, err: ParseError) {
        if let ParseError::UnexpectedEof = err {
            self.has_fatal_error = true
        }
        self.errors.push(err);
    }

    pub fn synchronize(&mut self, stop_at_tokens: &[TokenId]) {
        while !self.peek().is_eof() && !stop_at_tokens.contains(&self.peek().id) {
            self.advance();
        }
    }

    pub fn print_errors(&self) {
        for err in &self.errors {
            println!("{}", err);
        }
    }
}
