use super::lexer::{Token, TokenId};

#[derive(Debug)]
enum UnaryOperation {
    Return,
    Assignment,
}

#[derive(Debug)]
enum BinaryOperation {
    Add,
}

#[derive(Debug)]
enum Type {
    Int64,
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
}

#[derive(Debug)]
struct UnaryExpression {
    operation: UnaryOperation,
    operand: Box<Node>,
}

#[derive(Debug)]
struct BinaryExpression {
    operation: BinaryOperation,
    left: Box<Node>,
    right: Box<Node>,
}

#[derive(Debug)]
struct Variable {
    identifier: String,
    type_: Type,
    value: Box<Node>,
}

#[derive(Debug)]
struct Parameter {
    name: String,
    type_: Type,
}

#[derive(Debug)]
struct FunctionDefinition {
    identifier: String,
    return_type: Type,
    parameters: Vec<Parameter>,
    body: Vec<Node>,
}

#[derive(Debug)]
enum Value {
    Int64(i64),
}

#[derive(Debug)]
struct Constant {
    type_: Type,
    value: Value,
}

#[derive(Debug)]
pub enum Node {
    FunctionDefinition(FunctionDefinition),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
    Variable(Variable),
    Constant(Constant),
}

#[derive(Debug)]
pub struct Parser<I: Iterator<Item = Token>> {
    tokens: I,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Parser { tokens }
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn parse_next(&mut self) -> Option<Node> {
        if let Some(token) = self.next() {
            let node = match token {
                Token {
                    id: TokenId::Id(val),
                    ..
                } if val == "fn" => None,
                _ => None,
            };

            return node;
        }

        None
    }

    pub fn parse(&mut self) -> Option<Node> {
        self.parse_next()
    }
}
