#![allow(dead_code)]

use std::{fmt, iter::Peekable, str::FromStr};

use super::lexer::{EOF_TOKEN, Lexer, Token, TokenId};

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

#[derive(Debug)]
pub struct ParseContext<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
    errors: Vec<ParseError>,
    has_fatal_error: bool,
}

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

macro_rules! match_token_id {
    ($ctx:expr, $token:expr, $synchronize:expr, $( $arm_pattern:pat => $arm_body:expr ),* $(,)?) => {
        match $token.id {
            // Include all specific arms passed into the macro
            $( $arm_pattern => $arm_body, )*

            // Now, include the common fallback arms
            TokenId::Eof => {
                $ctx.log_error(ParseError::UnexpectedEof);
                return Node::Error
            },
            _ => {
                let err = ParseError::UnexpectedToken($token.clone());
                $ctx.log_error(err);
                $ctx.synchronize($synchronize);
                return Node::Error
            },
        }
    };
}

fn is_token_symbol(a: TokenId) -> impl Fn(&TokenId) -> bool {
    move |b| *b == a
}

fn parse_identifier<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
) -> Result<String, ParseError> {
    let token = ctx.advance();
    match token.id {
        TokenId::Id(s) => Ok(s),
        _ => Err(ParseError::UnexpectedToken(token.clone())),
    }
}

fn parse_param<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
) -> Result<Parameter, ParseError> {
    let name = parse_identifier(ctx)?;
    if !ctx.advance_required(is_token_symbol(TokenId::Colon)) {
        let err = ParseError::UnexpectedToken(ctx.peek().clone());
        return Err(err);
    }
    let type_ = parse_type(ctx)?;

    Ok(Parameter { name, type_ })
}

fn parse_fn_params<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
) -> Result<Vec<Parameter>, ParseError> {
    // Advance the parenthesis open
    if !ctx.advance_required(is_token_symbol(TokenId::ParenOpen)) {
        let err = ParseError::UnexpectedToken(ctx.peek().clone());
        return Err(err);
    }
    let mut params: Vec<Parameter> = vec![];

    // Loop until a closing parenthesis has been found e.g:
    // ()
    // (a: int)
    // (a: int, b: char)
    // (a: int, b: char,)
    loop {
        if ctx.has_fatal() || ctx.advance_optional(is_token_symbol(TokenId::ParenClose)) {
            break;
        }

        params.push(parse_param(ctx)?);

        ctx.advance_optional(is_token_symbol(TokenId::Comma));
    }

    Ok(params)
}

fn parse_type<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>) -> Result<Type, ParseError> {
    // Do not support function types yet

    let ident = parse_identifier(ctx);

    // Expect identifier to be a specific type keyword
    let mut type_ = ident?.parse()?;

    // Check for pointer symbol
    // DOES NOT RESOLVE MULTIPLE POINTERS
    if ctx.advance_optional(is_token_symbol(TokenId::Star)) {
        type_ = Type::Pointer {
            inner_type: Box::new(type_),
        };
    }
    Ok(type_)
}

fn parse_block<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>) -> Vec<Node> {
    let mut lines = vec![];

    if !ctx.advance_required_symbol_logged(TokenId::BraceOpen) {
        lines.push(Node::Error);
        return lines;
    }

    loop {
        if ctx.has_fatal() || ctx.advance_optional(is_token_symbol(TokenId::BraceClose)) {
            break;
        }

        let line = parse_statement(ctx);
        lines.push(line);
    }

    lines
}

fn parse_fn<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>) -> Node {
    if !ctx.advance_required_symbol_logged(TokenId::Fn) {
        return Node::Error;
    }

    let identifier = match parse_identifier(ctx) {
        Ok(s) => s,
        Err(e) => {
            ctx.log_error(e);
            return Node::Error;
        }
    };

    let parameters = match parse_fn_params(ctx) {
        Ok(s) => s,
        Err(e) => {
            ctx.log_error(e);
            return Node::Error;
        }
    };

    if !ctx.advance_required_symbol_logged(TokenId::Colon) {
        return Node::Error;
    }

    let return_type = match parse_type(ctx) {
        Ok(s) => s,
        Err(e) => {
            ctx.log_error(e);
            return Node::Error;
        }
    };

    let body = parse_block(ctx);

    let fn_def = FunctionDefinition {
        identifier,
        return_type,
        parameters,
        body,
    };
    Node::FunctionDefinition(fn_def)
}

fn parse_if_statement<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
    symbol: TokenId,
) -> Node {
    if !ctx.advance_required_symbol_logged(symbol) {
        return Node::Error;
    }

    let expression = parse_expression(ctx);
    let true_branch = parse_block(ctx);

    let next = ctx.peek();
    let false_branch = match next.id {
        TokenId::ElIf => vec![parse_if_statement(ctx, TokenId::ElIf)],
        TokenId::Else => {
            if ctx.advance_required_symbol_logged(TokenId::Else) {
                parse_block(ctx)
            } else {
                return Node::Error;
            }
        }
        _ => vec![],
    };

    let if_statement = IfStatement {
        expression: Box::new(expression),
        true_branch,
        false_branch,
    };

    Node::IfStatement(if_statement)
}

fn parse_unary_operation<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
    operation: UnaryOperation,
) -> Node {
    ctx.advance();

    let operand = Box::new(parse_expression(ctx));

    let expr = UnaryExpression { operation, operand };
    Node::UnaryExpression(expr)
}

fn parse_constant<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>) -> Node {
    let token = ctx.advance();

    let token_value = match_token_id!(ctx, token, &[TokenId::SemiColon, TokenId::Eof], TokenId::Number(ref s) => s);
    let number = token_value.parse::<i64>();

    match number {
        Ok(val) => Node::Constant(Constant {
            value: ConstantValue::Int64(val),
        }),
        Err(_) => {
            let err = ParseError::UnknownConstant(token.clone());
            ctx.log_error(err);
            Node::Error
        }
    }
}

fn parse_primary<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>) -> Node {
    let token = ctx.peek();
    match_token_id!(ctx, token, &[TokenId::SemiColon, TokenId::Eof], TokenId::Number(_) => parse_constant(ctx))
}

fn parse_expression_bp<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>, min_bp: u8) -> Node {
    let mut lhs = parse_primary(ctx);

    loop {
        // DO NOT CONSUME SEMICOLON
        // The end-of-expression token is consumed elsewhere
        // as all recursive calls need to end when a SemiColon is encountered
        // ------------------
        // Expressions are also valid within an if statement
        // Then the expression ends when a brace is encountered
        if lhs.is_error()
            || ctx.has_fatal()
            || ctx.expect(is_token_symbol(TokenId::SemiColon))
            || ctx.expect(is_token_symbol(TokenId::BraceOpen))
        {
            break;
        }

        let token = ctx.peek();
        let op_r = BinaryOperation::from_token_id(&token.id);

        match op_r {
            Ok(op) => {
                let (l_bp, r_bp) = op.binding_power();

                // If l_bp < min_bp then we should exit and return the primary
                // because the previous operator has precendence over the current
                if l_bp < min_bp {
                    break;
                }

                ctx.advance();
                let rhs = parse_expression_bp(ctx, r_bp);

                lhs = Node::BinaryExpression(BinaryExpression {
                    operation: op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                })
            }
            Err(err) => {
                ctx.log_error(err);
                return Node::Error;
            }
        }
    }

    lhs
}

fn parse_expression<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>) -> Node {
    // Perform Pratt parsing https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    parse_expression_bp(ctx, 0)
}

fn parse_statement<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>) -> Node {
    let token = ctx.peek();

    let (node, needs_semicolon) = match_token_id!(ctx, token, &[TokenId::SemiColon, TokenId::Eof],
        TokenId::Return => (parse_unary_operation(ctx, UnaryOperation::Return), true),
        TokenId::Assert => (parse_unary_operation(ctx, UnaryOperation::Assert), true),
        TokenId::If => (parse_if_statement(ctx, TokenId::If), false),
    );

    if needs_semicolon {
        // Consume SemiColon if statement needs to be closed with one
        if !ctx.advance_required_symbol_logged(TokenId::SemiColon) {
            return Node::Error;
        }
    }

    node
}

fn parse_next<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>) -> Node {
    let token = ctx.peek();

    match_token_id!(ctx, token, &[TokenId::SemiColon, TokenId::Eof], TokenId::Fn => parse_fn(ctx))
}

pub fn parse(lexer: Lexer) -> Node {
    let mut ctx = ParseContext::new(lexer);
    let node = parse_next(&mut ctx);

    if !ctx.errors.is_empty() {
        for err in ctx.errors {
            println!("{}", err);
        }
    }

    node
}

impl<I: Iterator<Item = Token>> ParseContext<I> {
    pub fn new(tokens: I) -> Self {
        ParseContext {
            tokens: tokens.peekable(),
            errors: vec![],
            has_fatal_error: false,
        }
    }

    fn advance(&mut self) -> Token {
        match self.tokens.next() {
            Some(token) => token,
            _ => EOF_TOKEN,
        }
    }

    fn peek(&mut self) -> &Token {
        match self.tokens.peek() {
            Some(token) => token,
            _ => &EOF_TOKEN,
        }
    }

    fn expect<F>(&mut self, matcher: F) -> bool
    where
        F: Fn(&TokenId) -> bool,
    {
        matches!(self.tokens.peek(), Some(Token { id, .. }) if matcher(id))
    }

    /// Expect the next token to match the given matching function
    /// and advance the tokenstream if it matches
    ///
    /// Will record the error if the encountered token does not match
    /// the expected token and returns false
    fn advance_required<F>(&mut self, matcher: F) -> bool
    where
        F: Fn(&TokenId) -> bool,
    {
        if self.expect(&matcher) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Will advance the token stream only if the expected token is found
    /// Returns whether expected token is found
    fn advance_optional<F>(&mut self, matcher: F) -> bool
    where
        F: Fn(&TokenId) -> bool,
    {
        let result = self.expect(&matcher);
        if result {
            let _ = self.advance();
        }
        result
    }

    fn advance_required_symbol_logged(&mut self, id: TokenId) -> bool {
        let result = self.advance_required(is_token_symbol(id));

        if !result {
            let s = ParseError::UnexpectedToken(self.peek().clone());
            self.log_error(s);
        }

        result
    }

    fn has_fatal(&self) -> bool {
        self.has_fatal_error
    }

    fn log_error(&mut self, err: ParseError) {
        if let ParseError::UnexpectedEof = err {
            self.has_fatal_error = true
        }
        self.errors.push(err);
    }

    fn synchronize(&mut self, stop_at_tokens: &[TokenId]) {
        while !self.peek().is_eof() && !stop_at_tokens.contains(&self.peek().id) {
            self.advance();
        }
    }
}
