use std::{fmt::Display, str::CharIndices};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenId {
    Id(String),
    Number(String),
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    BraceOpen,
    BraceClose,
    Colon,
    SemiColon,
    Comma,
    Unknown(char),
}

impl Display for TokenId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenId::Id(c) => write!(f, "ID({})", c),
            TokenId::Number(c) => {
                write!(f, "Number({})", c)
            }
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub id: TokenId,
    pub index: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token {{ id: {}, index: {} }}", self.id, self.index)
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    chars: CharIndices<'a>,
    curr: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut x = src.char_indices();
        let c = x.next();
        Self { chars: x, curr: c }
    }

    fn advance(&mut self) {
        self.curr = self.chars.next();
    }

    fn parse_id(&mut self) -> TokenId {
        let mut value = String::new();
        while let Some((_, ch)) = self.curr {
            if ch.is_alphanumeric() || ch == '_' {
                value.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        TokenId::Id(value)
    }

    fn parse_int(&mut self) -> TokenId {
        let mut value = String::new();
        while let Some((_, ch)) = self.curr {
            if ch.is_numeric() {
                value.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        TokenId::Number(value)
    }

    fn parse_char(&mut self, id: TokenId) -> TokenId {
        self.advance();
        id
    }

    fn parse_unknown(&mut self, c: char) -> TokenId {
        self.advance();
        TokenId::Unknown(c)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((index, c)) = self.curr {
            if c.is_whitespace() {
                self.advance();
                continue;
            }

            let token_id = match c {
                c if c.is_ascii_alphabetic() => self.parse_id(),
                c if c.is_ascii_digit() => self.parse_int(),
                '(' => self.parse_char(TokenId::ParenOpen),
                ')' => self.parse_char(TokenId::ParenClose),
                '[' => self.parse_char(TokenId::BracketOpen),
                ']' => self.parse_char(TokenId::BracketClose),
                '{' => self.parse_char(TokenId::BraceOpen),
                '}' => self.parse_char(TokenId::BraceClose),
                ':' => self.parse_char(TokenId::Colon),
                ';' => self.parse_char(TokenId::SemiColon),
                ',' => self.parse_char(TokenId::Comma),
                _ => self.parse_unknown(c),
            };
            return Some(Token {
                id: token_id,
                index,
            });
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case("(", TokenId::ParenOpen)]
    #[case(")", TokenId::ParenClose)]
    #[case("[", TokenId::BracketOpen)]
    #[case("]", TokenId::BracketClose)]
    #[case("{", TokenId::BraceOpen)]
    #[case("}", TokenId::BraceClose)]
    #[case(":", TokenId::Colon)]
    #[case(";", TokenId::SemiColon)]
    #[case(",", TokenId::Comma)]
    fn test_lexer_single_token(#[case] src: &str, #[case] expected: TokenId) {
        let a = Token {
            id: expected,
            index: 0,
        };
        let result = Lexer::new(src).next();
        assert_eq!(result, Some(a));
    }

    #[test]
    fn test_lexer_token_identifier() {
        let expected = Token {
            id: TokenId::Id("main".to_owned()),
            index: 0,
        };
        let result = Lexer::new("main\n").next();
        assert_eq!(result, Some(expected));
    }

    #[test]
    fn test_lexer_token_number() {
        let expected = Token {
            id: TokenId::Number("123".to_owned()),
            index: 0,
        };
        let result = Lexer::new("123\n").next();
        assert_eq!(result, Some(expected));
    }
}
