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
        inner_type: Box<Type>
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
    operation: UnaryOperation,
    operand: Box<Node>,
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
    identifier: String,
    return_type: Type,
    parameters: Vec<Parameter>,
    body: Vec<Node>,
}

#[derive(Debug)]
pub enum ConstantValue {
    Int64(i64),
}

#[derive(Debug)]
pub struct Constant {
    value: ConstantValue,
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
        let x =  self.tokens.next();
        x
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
            Token { id: TokenId::Id(s), .. } => s,
            unexpected => panic!("Unreachable code detected - found {} after matching for ID token", unexpected),
        }
    }

    fn parse_param(&mut self) -> Parameter {
        let name = self.parse_identifier();
        self.expect_and_advance(is_token_symbol(TokenId::Colon));
        let type_ = self.parse_type();

        Parameter{name, type_}
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

            if self.advance_if_expected(is_token_symbol(TokenId::ParenClose))  {
                break;
            }
            
            params.push(self.parse_param());

            self.advance_if_expected(is_token_symbol(TokenId::Comma));
        }

        params
    }

    fn parse_type(&mut self) -> Type {
        // Do not support fn types yet

        let ident = self.parse_identifier();

        // Expect identifier to be a specific type keyword
        let type_ = ident.parse().expect("Unexpected type");

        // Check for pointer symbol
        // DOES NOT RESOLVE MULTIPLE POINTERS
        if self.advance_if_expected(is_token_symbol(TokenId::Asterisk)) {
            return Type::Pointer { inner_type: Box::new(type_) };
        }
        type_
    }

    fn parse_fn_body(&mut self) -> Vec<Node> {
        vec![]
    }

    fn parse_fn(&mut self) -> Node {
        let identifier = self.parse_identifier();
        
        let parameters = self.parse_fn_params();

        self.advance_token_symbol(TokenId::Colon);
        let return_type = self.parse_type();

        let body = self.parse_fn_body();

        let fn_def = FunctionDefinition{identifier, return_type, parameters, body};
        Node::FunctionDefinition(fn_def)
    }

    fn parse_next(&mut self) -> Option<Node> {
        if let Some(token) = self.advance() {
            let node = match token {
                Token {
                    id: TokenId::Id(val),
                    ..
                } if val == "fn" => self.parse_fn(),
                _ => panic!(),
            };

            return Some(node);
        }

        None
    }

    pub fn parse(&mut self) -> Option<Node> {
        self.parse_next()
    }
}
