#![allow(dead_code)]

use std::str::FromStr;

use crate::bonobo::lexer::TokenId;

use super::error::ParseError;

pub trait BindingPower {
    fn binding_power(&self) -> (u8, u8);
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Modulo,
    Division,
    Equals,
    Assignment,
}

impl BindingPower for Operator {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Operator::Assignment => (3, 4),
            Operator::Equals => (17, 18),
            Operator::Add | Operator::Subtract => (23, 24),
            Operator::Multiply | Operator::Modulo | Operator::Division => (25, 26),
        }
    }
}

impl TryFrom<&TokenId> for Operator {
    type Error = ParseError;

    fn try_from(token_id: &TokenId) -> Result<Self, Self::Error> {
        match token_id {
            TokenId::Plus => Ok(Self::Add),
            TokenId::Minus => Ok(Self::Subtract),
            TokenId::Star => Ok(Self::Multiply),
            TokenId::Percent => Ok(Self::Modulo),
            TokenId::Slash => Ok(Self::Division),
            TokenId::EqualsEquals => Ok(Self::Equals),
            TokenId::Equals => Ok(Self::Assignment),
            _ => Err(ParseError::UnknownOperator(token_id.clone())),
        }
    }
}

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

impl TryFrom<Operator> for BinaryOperation {
    type Error = ParseError;

    fn try_from(operator: Operator) -> Result<Self, Self::Error> {
        match operator {
            Operator::Add => Ok(Self::Add),
            Operator::Subtract => Ok(Self::Subtract),
            Operator::Multiply => Ok(Self::Multiply),
            Operator::Modulo => Ok(Self::Modulo),
            Operator::Division => Ok(Self::Division),
            Operator::Equals => Ok(Self::Equals),
            _ => Err(ParseError::UnknownBinaryOperation(operator)),
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
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    pub identifier: String,
    pub type_: Type,
}

#[derive(Debug, PartialEq)]
pub struct VariableAssignment {
    pub identifier: String,
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
pub struct FunctionCall {
    pub identifier: String,
    pub arguments: Vec<Node>,
}

#[derive(Debug, PartialEq)]
pub struct AstProgram {
    pub functions: Vec<Node>,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    FunctionDefinition(FunctionDefinition),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
    VariableDeclaration(VariableDeclaration),
    VariableAssignment(VariableAssignment),
    IfStatement(IfStatement),
    Identifier(Identifier),
    Constant(Constant),
    FunctionCall(FunctionCall),
    Error,
}

impl Node {
    pub fn is_error(&self) -> bool {
        *self == Node::Error
    }

    pub fn as_identifier(&self) -> Option<&String> {
        if let Node::Identifier(ident) = self {
            Some(&ident.name)
        } else {
            None
        }
    }
}
