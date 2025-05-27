#![allow(dead_code)]

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
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ParseError {}
