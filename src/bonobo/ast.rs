#![allow(dead_code)]

use std::{fmt, iter::Peekable, str::FromStr};

use super::lexer::{Token, TokenId};

#[derive(Debug)]
pub enum UnaryOperation {
    Return,
    Assert,
}

#[derive(Debug)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Modulo,
    Division,
    Equals,
}

impl BinaryOperation {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            BinaryOperation::Equals => (1, 2),
            BinaryOperation::Add | BinaryOperation::Subtract => (3, 4),
            BinaryOperation::Multiply | BinaryOperation::Modulo | BinaryOperation::Division => {
                (5, 6)
            }
        }
    }

    fn from_token_id(token_id: &TokenId) -> Result<Self, ParseError> {
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
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(Type::Int64),
            "char" => Ok(Type::Char),
            _ => Err(ParseError::UnknownType(s.into())),
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
    pub operation: BinaryOperation,
    pub left: Box<Node>,
    pub right: Box<Node>,
}

#[derive(Debug)]
pub struct IfStatement {
    pub expression: Box<Node>,
    pub true_branch: Vec<Node>,
    pub false_branch: Vec<Node>,
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
    IfStatement(IfStatement),
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
    UnknownType(String),
    UnknownOperator(TokenId),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ParseError {}

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

    fn advance_existing(&mut self) -> Result<Token, ParseError> {
        match self.advance() {
            Some(token) => Ok(token),
            _ => Err(ParseError::UnexpectedEof),
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn peek_existing(&mut self) -> Result<&Token, ParseError> {
        match self.peek() {
            Some(token) => Ok(token),
            _ => Err(ParseError::UnexpectedEof),
        }
    }

    fn expect<F>(&mut self, matcher: F) -> bool
    where
        F: Fn(&TokenId) -> bool,
    {
        matches!(self.tokens.peek(), Some(Token { id, .. }) if matcher(id))
    }

    /// Expect the next token to match the given matching function
    ///
    /// # Errors
    ///
    /// This function will return an error if the token does not match the expected token,
    /// or if an unexpected EOF was encountered.
    fn advance_required<F>(&mut self, matcher: F) -> Result<Token, ParseError>
    where
        F: Fn(&TokenId) -> bool,
    {
        if self.expect(&matcher) {
            Ok(self.advance().unwrap())
        } else {
            match self.tokens.peek() {
                Some(s) => Err(ParseError::UnexpectedToken(s.clone())),
                None => Err(ParseError::UnexpectedEof),
            }
        }
    }

    /// Will advance the token stream if the expected token is found
    /// Returns whether expected token is found
    fn advance_optional<F>(&mut self, matcher: F) -> bool
    where
        F: Fn(&TokenId) -> bool,
    {
        let result = self.expect(&matcher);
        if result {
            self.advance();
        }
        result
    }

    fn advance_required_symbol(&mut self, id: TokenId) -> Result<Token, ParseError> {
        self.advance_required(is_token_symbol(id))
    }

    fn parse_identifier(&mut self) -> Result<String, ParseError> {
        let token = self.advance_existing()?;
        match token.id {
            TokenId::Id(s) => Ok(s),
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    fn parse_number(&mut self) -> Result<String, ParseError> {
        let token = self.advance_existing()?;
        match token.id {
            TokenId::Number(s) => Ok(s),
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    fn parse_param(&mut self) -> Result<Parameter, ParseError> {
        let name = self.parse_identifier()?;
        self.advance_required_symbol(TokenId::Colon)?;
        let type_ = self.parse_type()?;

        Ok(Parameter { name, type_ })
    }

    fn parse_fn_params(&mut self) -> Result<Vec<Parameter>, ParseError> {
        // Advance the parenthesis open
        self.advance_required_symbol(TokenId::ParenOpen)?;
        let mut params: Vec<Parameter> = vec![];

        // Loop until a closing parenthesis has been found e.g:
        // ()
        // (a: int)
        // (a: int, b: char)
        // (a: int, b: char,)
        loop {
            if self.advance_optional(is_token_symbol(TokenId::ParenClose)) {
                break;
            }

            params.push(self.parse_param()?);

            self.advance_optional(is_token_symbol(TokenId::Comma));
        }

        Ok(params)
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        // Do not support function types yet

        let ident = self.parse_identifier();

        // Expect identifier to be a specific type keyword
        let mut type_ = ident?.parse()?;

        // Check for pointer symbol
        // DOES NOT RESOLVE MULTIPLE POINTERS
        if self.advance_optional(is_token_symbol(TokenId::Star)) {
            type_ = Type::Pointer {
                inner_type: Box::new(type_),
            };
        }
        Ok(type_)
    }

    fn parse_block(&mut self) -> Result<Vec<Node>, ParseError> {
        self.advance_required_symbol(TokenId::BraceOpen)?;
        let mut lines = vec![];

        loop {
            if self.advance_optional(is_token_symbol(TokenId::BraceClose)) {
                break;
            }

            let line = self.parse_statement()?;
            lines.push(line);
        }

        Ok(lines)
    }

    fn parse_fn(&mut self) -> Result<Node, ParseError> {
        self.advance_required_symbol(TokenId::Fn)?;

        let identifier = self.parse_identifier()?;

        let parameters = self.parse_fn_params()?;

        self.advance_required_symbol(TokenId::Colon)?;
        let return_type = self.parse_type()?;

        let body = self.parse_block()?;

        let fn_def = FunctionDefinition {
            identifier,
            return_type,
            parameters,
            body,
        };
        Ok(Node::FunctionDefinition(fn_def))
    }

    fn parse_if_statement(&mut self, symbol: TokenId) -> Result<Node, ParseError> {
        self.advance_required_symbol(symbol)?;

        let expression = self.parse_expression()?;
        let true_branch = self.parse_block()?;

        let next = self.peek_existing()?;
        let false_branch = match next.id {
            TokenId::ElIf => Ok(vec![self.parse_if_statement(TokenId::ElIf)?]),
            TokenId::Else => {
                self.advance_required_symbol(TokenId::Else)?;
                self.parse_block()
            }
            _ => Ok(vec![]),
        }?;

        let if_statement = IfStatement {
            expression: Box::new(expression),
            true_branch,
            false_branch,
        };

        Ok(Node::IfStatement(if_statement))
    }

    fn parse_unary_operation(&mut self, operation: UnaryOperation) -> Result<Node, ParseError> {
        self.advance();

        let operand_node = self.parse_expression()?;
        let operand = Box::new(operand_node);

        let expr = UnaryExpression { operation, operand };
        Ok(Node::UnaryExpression(expr))
    }

    fn parse_constant(&mut self) -> Result<Node, ParseError> {
        let token_value = self.parse_number()?;
        let number = token_value.parse::<i64>();

        match number {
            Ok(val) => Ok(Node::Constant(Constant {
                value: ConstantValue::Int64(val),
            })),
            Err(_) => Err(ParseError::UnknownConstant(token_value)),
        }
    }

    fn parse_primary(&mut self) -> Result<Node, ParseError> {
        let token = self.peek_existing()?;
        match token.id {
            TokenId::Number(_) => self.parse_constant(),
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    fn parse_expression_bp(&mut self, min_bp: u8) -> Result<Node, ParseError> {
        let mut lhs = self.parse_primary()?;

        loop {
            // DO NOT CONSUME SEMICOLON
            // The end-of-expression token is consumed elsewhere
            // as all recursive calls need to end when a SemiColon is encountered
            // ------------------
            // Expressions are also valid within an if statement
            // Then the expression ends when a brace is encountered
            if self.expect(is_token_symbol(TokenId::SemiColon))
                || self.expect(is_token_symbol(TokenId::BraceOpen))
            {
                break;
            }

            let token = self.peek_existing()?;
            let op = BinaryOperation::from_token_id(&token.id)?;

            let (l_bp, r_bp) = op.binding_power();

            // If l_bp < min_bp then we should exit and return the primary
            // because the previous operator has precendence over the current
            if l_bp < min_bp {
                break;
            }

            self.advance_existing()?;
            let rhs = self.parse_expression_bp(r_bp)?;

            lhs = Node::BinaryExpression(BinaryExpression {
                operation: op,
                left: Box::new(lhs),
                right: Box::new(rhs),
            })
        }

        Ok(lhs)
    }

    fn parse_expression(&mut self) -> Result<Node, ParseError> {
        // Perform Pratt parsing https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
        self.parse_expression_bp(0)
    }

    fn parse_statement(&mut self) -> Result<Node, ParseError> {
        let token = self.peek_existing()?;

        let (node, needs_semicolon) = match token.id {
            TokenId::Return => (self.parse_unary_operation(UnaryOperation::Return), true),
            TokenId::Assert => (self.parse_unary_operation(UnaryOperation::Assert), true),
            TokenId::If => (self.parse_if_statement(TokenId::If), false),
            _ => (Err(ParseError::UnexpectedToken(token.clone())), false),
        };

        if needs_semicolon {
            // Consume SemiColon if statement needs to be closed with one
            self.advance_required(is_token_symbol(TokenId::SemiColon))?;
        }

        node
    }

    fn parse_next(&mut self) -> Result<Node, ParseError> {
        if let Some(token) = self.peek() {
            let node = match token.id {
                TokenId::Fn => self.parse_fn()?,
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
