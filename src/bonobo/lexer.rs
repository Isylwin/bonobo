use std::{fmt::Display, iter::Peekable, str::CharIndices};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenId {
    Fn,
    If,
    ElIf,
    Else,
    Return,
    Assert,
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
    Star,
    Plus,
    Minus,
    Percent,
    Slash,
    EqualsEquals,
    Unknown(String),
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

#[derive(Debug, PartialEq, Clone)]
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
    chars: Peekable<CharIndices<'a>>,
    curr: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut x = src.char_indices().peekable();
        let c = x.next();
        Self { chars: x, curr: c }
    }

    fn advance(&mut self) {
        self.curr = self.chars.next();
    }

    fn peek(&mut self) -> Option<&(usize, char)> {
        self.chars.peek()
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

        match value.as_str() {
            "fn" => TokenId::Fn,
            "if" => TokenId::If,
            "elif" => TokenId::ElIf,
            "else" => TokenId::Else,
            "return" => TokenId::Return,
            "assert" => TokenId::Assert,
            _ => TokenId::Id(value),
        }
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

    fn parse_equals(&mut self) -> TokenId {
        let id = match self.peek() {
            Some((_, '=')) => {
                self.advance();
                TokenId::EqualsEquals
            }
            Some((_, c)) => TokenId::Unknown(format!("={}", c)),
            _ => TokenId::Unknown("\0".into()),
        };

        self.advance();
        id
    }

    fn parse_unknown(&mut self, c: char) -> TokenId {
        self.advance();
        TokenId::Unknown(c.into())
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
                '*' => self.parse_char(TokenId::Star),
                '+' => self.parse_char(TokenId::Plus),
                '-' => self.parse_char(TokenId::Minus),
                '%' => self.parse_char(TokenId::Percent),
                '/' => self.parse_char(TokenId::Slash),
                '=' => self.parse_equals(),
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
    #[case::paren_open("(", TokenId::ParenOpen)]
    #[case::paren_close(")", TokenId::ParenClose)]
    #[case::bracket_open("[", TokenId::BracketOpen)]
    #[case::bracket_close("]", TokenId::BracketClose)]
    #[case::brace_open("{", TokenId::BraceOpen)]
    #[case::brace_close("}", TokenId::BraceClose)]
    #[case::colon(":", TokenId::Colon)]
    #[case::semi_colon(";", TokenId::SemiColon)]
    #[case::comma(",", TokenId::Comma)]
    #[case::star("*", TokenId::Star)]
    #[case::plus("+", TokenId::Plus)]
    #[case::plus("-", TokenId::Minus)]
    #[case::percent("%", TokenId::Percent)]
    #[case::slash("/", TokenId::Slash)]
    #[case::equalsequals("==", TokenId::EqualsEquals)]
    #[case::equalsgt("=>", TokenId::Unknown("=>".into()))]
    #[case::fn_("fn", TokenId::Fn)]
    #[case::if_("if", TokenId::If)]
    #[case::elif_("elif", TokenId::ElIf)]
    #[case::else_("else", TokenId::Else)]
    #[case::return_("return", TokenId::Return)]
    #[case::assert_("assert", TokenId::Assert)]
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
            id: TokenId::Id("main".into()),
            index: 0,
        };
        let result = Lexer::new("main\n").next();
        assert_eq!(result, Some(expected));
    }

    #[test]
    fn test_lexer_token_number() {
        let expected = Token {
            id: TokenId::Number("123".into()),
            index: 0,
        };
        let result = Lexer::new("123\n").next();
        assert_eq!(result, Some(expected));
    }
}
