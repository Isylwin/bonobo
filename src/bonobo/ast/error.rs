use core::fmt;

use crate::bonobo::lexer::{Token, TokenId};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    UnexpectedEof,
    UnknownConstant(Token),
    UnknownType(String),
    UnknownOperator(TokenId),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token: {}", token),
            ParseError::UnexpectedEof => write!(f, "Unexpected End-of-file encountered"),
            ParseError::UnknownConstant(token) => write!(f, "Unknown constant: {}", token),
            ParseError::UnknownType(type_name) => write!(f, "Unknown type name: {}", type_name),
            ParseError::UnknownOperator(token_id) => write!(f, "Unknown operator: {}", token_id),
        }
    }
}

impl std::error::Error for ParseError {}
