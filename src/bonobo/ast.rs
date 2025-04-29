#![allow(dead_code)]

use std::{iter::Peekable, str::FromStr};

use super::lexer::{Token, TokenId};

#[derive(Debug)]
pub enum UnaryOperation {
    Return,
    Assignment,
}

#[derive(Debug)]
pub enum BinaryOperation {
    Add,
}

#[derive(Debug)]
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
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(Type::Int64),
            "char" => Ok(Type::Char),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct UnaryExpression {
    pub operation: UnaryOperation,
    pub operand: Box<Node>,
}

#[derive(Debug)]
pub struct BinaryExpression {
    operation: BinaryOperation,
    left: Box<Node>,
    right: Box<Node>,
}

#[derive(Debug)]
pub struct Variable {
    identifier: String,
    type_: Type,
    value: Box<Node>,
}

#[derive(Debug)]
pub struct Parameter {
    name: String,
    type_: Type,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub identifier: String,
    pub return_type: Type,
    pub parameters: Vec<Parameter>,
    pub body: Vec<Node>,
}

#[derive(Debug)]
pub enum ConstantValue {
    Int64(i64),
}

#[derive(Debug)]
pub struct Constant {
    pub value: ConstantValue,
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
    tokens: Peekable<I>,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    UnexpectedEof,
    UnknownConstant(String), // TODO should be a token
}

fn is_token_symbol(a: TokenId) -> impl Fn(&TokenId) -> bool {
    move |b| *b == a
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
        }
    }

    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn expect<F>(&mut self, matcher: F) -> bool
    where
        F: Fn(&TokenId) -> bool,
    {
        matches!(self.tokens.peek(), Some(Token { id, .. }) if matcher(id))
    }

    fn expect_and_advance<F>(&mut self, matcher: F) -> Token
    where
        F: Fn(&TokenId) -> bool,
    {
        if self.expect(&matcher) {
            self.advance().unwrap()
        } else {
            panic!("The token: {:?} was unexpected", self.tokens.peek());
        }
    }

    /// Will advance the token stream if the expected token is found
    /// Returns whether expected token is found
    fn advance_if_expected<F>(&mut self, matcher: F) -> bool
    where
        F: Fn(&TokenId) -> bool,
    {
        let result = self.expect(&matcher);
        if result {
            self.advance();
        }
        result
    }

    fn advance_token_symbol(&mut self, id: TokenId) -> Token {
        self.expect_and_advance(is_token_symbol(id))
    }

    fn parse_identifier(&mut self) -> String {
        match self.expect_and_advance(|id| matches!(id, TokenId::Id(_))) {
            Token {
                id: TokenId::Id(s), ..
            } => s,
            unexpected => panic!(
                "Unreachable code detected - found {} after matching for ID token",
                unexpected
            ),
        }
    }

    fn parse_number(&mut self) -> String {
        match self.expect_and_advance(|id| matches!(id, TokenId::Number(_))) {
            Token {
                id: TokenId::Number(s),
                ..
            } => s,
            unexpected => panic!(
                "Unreachable code detected - found {} after matching for Number token",
                unexpected
            ),
        }
    }

    fn parse_param(&mut self) -> Parameter {
        let name = self.parse_identifier();
        self.advance_token_symbol(TokenId::Colon);
        let type_ = self.parse_type();

        Parameter { name, type_ }
    }

    fn parse_fn_params(&mut self) -> Vec<Parameter> {
        // Advance the parenthesis open
        self.advance_token_symbol(TokenId::ParenOpen);
        let mut params: Vec<Parameter> = vec![];

        // Loop until a closing parenthesis has been found e.g:
        // ()
        // (a: int)
        // (a: int, b: char)
        // (a: int, b: char,)
        loop {
            if self.advance_if_expected(is_token_symbol(TokenId::ParenClose)) {
                break;
            }

            params.push(self.parse_param());

            self.advance_if_expected(is_token_symbol(TokenId::Comma));
        }

        params
    }

    fn parse_type(&mut self) -> Type {
        // Do not support function types yet

        let ident = self.parse_identifier();

        // Expect identifier to be a specific type keyword
        let type_ = ident.parse().expect("Unexpected type");

        // Check for pointer symbol
        // DOES NOT RESOLVE MULTIPLE POINTERS
        if self.advance_if_expected(is_token_symbol(TokenId::Asterisk)) {
            return Type::Pointer {
                inner_type: Box::new(type_),
            };
        }
        type_
    }

    fn parse_fn_body(&mut self) -> Vec<Node> {
        self.advance_token_symbol(TokenId::BraceOpen);
        let mut lines = vec![];

        loop {
            if self.advance_if_expected(is_token_symbol(TokenId::BraceClose)) {
                break;
            }

            let line = self.parse_statement();
            lines.push(line.unwrap());
        }

        lines
    }

    fn parse_fn(&mut self) -> Node {
        self.advance_token_symbol(TokenId::Fn);

        let identifier = self.parse_identifier();

        let parameters = self.parse_fn_params();

        self.advance_token_symbol(TokenId::Colon);
        let return_type = self.parse_type();

        let body = self.parse_fn_body();

        let fn_def = FunctionDefinition {
            identifier,
            return_type,
            parameters,
            body,
        };
        Node::FunctionDefinition(fn_def)
    }

    fn parse_return(&mut self) -> Node {
        self.advance_token_symbol(TokenId::Return);

        let operation = UnaryOperation::Return;
        let operand_node = self.parse_expression();
        let operand = Box::new(operand_node.unwrap());

        // Consume the expected ; after the expression
        self.advance_if_expected(is_token_symbol(TokenId::SemiColon));

        let expr = UnaryExpression { operation, operand };
        Node::UnaryExpression(expr)
    }

    fn parse_constant(&mut self) -> Result<Node, ParseError> {
        let pre_parsed = self.parse_number();
        let number = pre_parsed.parse::<i64>();

        match number {
            Ok(val) => Ok(Node::Constant(Constant {
                value: ConstantValue::Int64(val),
            })),
            Err(_) => Err(ParseError::UnknownConstant(pre_parsed)),
        }
    }

    fn parse_expression(&mut self) -> Result<Node, ParseError> {
        if let Some(token) = self.peek() {
            let node = match token {
                Token {
                    id: TokenId::Number(_),
                    ..
                } => self.parse_constant(),
                _ => return Err(ParseError::UnexpectedToken(token.clone())),
            };
            return node;
        }
        Err(ParseError::UnexpectedEof)
    }

    fn parse_statement(&mut self) -> Result<Node, ParseError> {
        if let Some(token) = self.peek() {
            let node = match token {
                Token {
                    id: TokenId::Return,
                    ..
                } => self.parse_return(),
                _ => return Err(ParseError::UnexpectedToken(token.clone())),
            };

            return Ok(node);
        }

        Err(ParseError::UnexpectedEof)
    }

    fn parse_next(&mut self) -> Result<Node, ParseError> {
        if let Some(token) = self.peek() {
            let node = match token {
                Token {
                    id: TokenId::Fn, ..
                } => self.parse_fn(),
                _ => return Err(ParseError::UnexpectedToken(token.clone())),
            };

            return Ok(node);
        }

        Err(ParseError::UnexpectedEof)
    }

    pub fn parse(&mut self) -> Result<Node, ParseError> {
        self.parse_next()
    }
}
