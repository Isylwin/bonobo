#![allow(dead_code)]

use std::str::FromStr;

use crate::bonobo::lexer::TokenId;

use super::error::ParseError;

#[derive(Debug, PartialEq)]
pub enum UnaryOperation {
    Return,
    Assert,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Modulo,
    Division,
    Equals,
}

impl BinaryOperation {
    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            BinaryOperation::Equals => (1, 2),
            BinaryOperation::Add | BinaryOperation::Subtract => (3, 4),
            BinaryOperation::Multiply | BinaryOperation::Modulo | BinaryOperation::Division => {
                (5, 6)
            }
        }
    }

    pub fn from_token_id(token_id: &TokenId) -> Result<Self, ParseError> {
        match token_id {
            TokenId::Plus => Ok(Self::Add),
            TokenId::Minus => Ok(Self::Subtract),
            TokenId::Star => Ok(Self::Multiply),
            TokenId::Percent => Ok(Self::Modulo),
            TokenId::Slash => Ok(Self::Division),
            TokenId::EqualsEquals => Ok(Self::Equals),
            _ => Err(ParseError::UnknownOperator(token_id.clone())),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int64,
    Char,
    Pointer {
        inner_type: Box<Type>,
    },
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
}

impl FromStr for Type {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(Type::Int64),
            "char" => Ok(Type::Char),
            _ => Err(ParseError::UnknownType(s.into())),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpression {
    pub operation: UnaryOperation,
    pub operand: Box<Node>,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpression {
    pub operation: BinaryOperation,
    pub left: Box<Node>,
    pub right: Box<Node>,
}

#[derive(Debug, PartialEq)]
pub struct IfStatement {
    pub expression: Box<Node>,
    pub true_branch: Vec<Node>,
    pub false_branch: Vec<Node>,
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub identifier: String,
    pub type_: Type,
    pub value: Box<Node>,
}

#[derive(Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDefinition {
    pub identifier: String,
    pub return_type: Type,
    pub parameters: Vec<Parameter>,
    pub body: Vec<Node>,
}

#[derive(Debug, PartialEq)]
pub enum ConstantValue {
    Int64(i64),
}

#[derive(Debug, PartialEq)]
pub struct Constant {
    pub value: ConstantValue,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    FunctionDefinition(FunctionDefinition),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
    IfStatement(IfStatement),
    Variable(Variable),
    Constant(Constant),
    Error,
}

impl Node {
    pub fn is_error(&self) -> bool {
        *self == Node::Error
    }
}
