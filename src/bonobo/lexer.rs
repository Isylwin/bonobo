use std::{fmt, iter::Peekable, str::CharIndices};

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

impl fmt::Display for TokenId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenId::Id(c) => write!(f, "ID({})", c),
            TokenId::Number(c) => {
                write!(f, "Number({})", c)
            }
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
}

impl Span {
    fn new(line: usize, column: usize) -> Span {
        Span { line, column }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Line {}, Column {}", self.line, self.column,)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub id: TokenId,
    pub span: Span,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Token {{ id: {}, span: {} }}", self.id, self.span)
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    chars: Peekable<CharIndices<'a>>,
    c_line: usize,
    c_column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let x = src.char_indices().peekable();
        let c_line = 1;
        let c_column = 0;
        Self {
            chars: x,
            c_column,
            c_line,
        }
    }

    fn advance(&mut self) -> Option<(Span, char)> {
        let next = self.chars.next();

        (self.c_line, self.c_column) = match next {
            Some((_, '\n')) => (self.c_line + 1, 0),
            _ => (self.c_line, self.c_column + 1),
        };

        next.map(|(_, c)| (Span::new(self.c_line, self.c_column), c))
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn parse_id(&mut self, curr: char) -> TokenId {
        let mut value = String::from(curr);

        while let Some(ch) = self.peek() {
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

    fn parse_int(&mut self, curr: char) -> TokenId {
        let mut value = String::from(curr);
        while let Some(ch) = self.peek() {
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
        id
    }

    fn parse_equals(&mut self) -> TokenId {
        match self.peek() {
            Some('=') => {
                self.advance();
                TokenId::EqualsEquals
            }
            Some(c) => TokenId::Unknown(format!("={}", c)),
            _ => TokenId::Unknown("\0".into()),
        }
    }

    fn parse_unknown(&mut self, c: char) -> TokenId {
        TokenId::Unknown(c.into())
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((span, c)) = self.advance() {
            if c.is_whitespace() {
                continue;
            }

            let token_id = match c {
                c if c.is_ascii_alphabetic() || c == '_' => self.parse_id(c),
                c if c.is_ascii_digit() => self.parse_int(c),
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
            return Some(Token { id: token_id, span });
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    fn cr_def_span() -> Span {
        Span::new(1, 1)
    }

    fn cr_id_token(val: &str, line: usize, column: usize) -> Token {
        cr_token(TokenId::Id(val.into()), line, column)
    }

    fn cr_token(id: TokenId, line: usize, column: usize) -> Token {
        Token {
            id,
            span: Span { line, column },
        }
    }

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
            span: cr_def_span(),
        };
        let result = Lexer::new(src).next();
        assert_eq!(result, Some(a));
    }

    #[test]
    fn test_lexer_token_identifier() {
        let expected = Token {
            id: TokenId::Id("main".into()),
            span: cr_def_span(),
        };
        let result = Lexer::new("main\n").next();
        assert_eq!(result, Some(expected));
    }

    #[test]
    fn test_lexer_token_number() {
        let expected = Token {
            id: TokenId::Number("123".into()),
            span: cr_def_span(),
        };
        let result = Lexer::new("123\n").next();
        assert_eq!(result, Some(expected));
    }

    #[test]
    fn test_sample_string() {
        let expected = vec![
            cr_token(TokenId::ParenOpen, 1, 1),
            cr_id_token("_hi", 1, 2),
            cr_token(TokenId::ParenClose, 1, 5),
            cr_id_token("bye", 2, 1),
        ];
        let result: Vec<Token> = Lexer::new("(_hi)\nbye").collect();
        assert_eq!(result, expected);
    }
}
