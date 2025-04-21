use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum TokenId {
    Id(Vec<u8>),
    Number(Vec<u8>),
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    BraceOpen,
    BraceClose,
    Colon,
    SemiColon,
    Comma,
}

impl Display for TokenId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenId::Id(c) => write!(f, "ID({})", String::from_utf8(c.to_vec()).unwrap()),
            TokenId::Number(c) => {
                write!(f, "Number({})", String::from_utf8(c.to_vec()).unwrap())
            }
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Debug)]
pub struct Token {
    id: TokenId,
    index: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token {{ id: {}, index: {} }}", self.id, self.index)
    }
}

pub struct Lexer {
    src: Vec<u8>,
    i: usize,
    c: u8,
}

impl Lexer {
    pub fn new(src: String) -> Self {
        let mut x = src.into_bytes();
        // Make sure it is null terminated...
        x.push(0);
        let i = 0;
        let c = x[i];
        Self { src: x, i: i, c: c }
    }

    fn advance(&mut self) {
        self.i += 1;
        self.c = self.src[self.i];
    }

    fn parse_char(&mut self, token_id: TokenId) -> Token {
        let index = self.i;
        self.advance();
        Token {
            id: token_id,
            index,
        }
    }

    fn parse_id(&mut self) -> Token {
        let start = self.i;
        while self.c.is_ascii_alphanumeric() {
            self.advance();
        }
        let end = self.i;

        let v = &self.src[start..end];

        Token {
            id: TokenId::Id(v.to_vec()),
            index: start,
        }
    }

    fn parse_int(&mut self) -> Token {
        let start = self.i;
        while self.c.is_ascii_digit() {
            self.advance();
        }
        let end = self.i;

        let v = &self.src[start..end];

        Token {
            id: TokenId::Number(v.to_vec()),
            index: start,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.c.is_ascii_whitespace() {
            self.advance();
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let size = self.src.len();
        if self.c == 0 || self.i >= size {
            return None;
        }

        self.skip_whitespace();

        let c = self.c;

        let token = match c {
            c if c.is_ascii_alphabetic() => self.parse_id(),
            c if c.is_ascii_digit() => self.parse_int(),
            b'(' => self.parse_char(TokenId::ParenOpen),
            b')' => self.parse_char(TokenId::ParenClose),
            b'[' => self.parse_char(TokenId::BracketOpen),
            b']' => self.parse_char(TokenId::BracketClose),
            b'{' => self.parse_char(TokenId::BraceOpen),
            b'}' => self.parse_char(TokenId::BraceClose),
            b':' => self.parse_char(TokenId::Colon),
            b';' => self.parse_char(TokenId::SemiColon),
            b',' => self.parse_char(TokenId::Comma),
            _ => panic!("Illegal character {}", self.c),
        };

        Some(token)
    }
}

#[cfg(test)]
mod tests {}
