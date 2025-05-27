#![allow(dead_code)]

use std::{fmt, iter::Peekable, str::FromStr};

use super::lexer::{EOF_TOKEN, Lexer, Token, TokenId};

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
pub struct ParseContext<I: Iterator<Item = Token>> {
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

fn parse_identifier<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
) -> Result<String, ParseError> {
    let token = ctx.advance();
    match token.id {
        TokenId::Id(s) => Ok(s),
        _ => Err(ParseError::UnexpectedToken(token.clone())),
    }
}

fn parse_number<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
) -> Result<String, ParseError> {
    let token = ctx.advance();
    match token.id {
        TokenId::Number(s) => Ok(s),
        _ => Err(ParseError::UnexpectedToken(token.clone())),
    }
}

fn parse_param<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
) -> Result<Parameter, ParseError> {
    let name = parse_identifier(ctx)?;
    ctx.advance_required_symbol(TokenId::Colon)?;
    let type_ = parse_type(ctx)?;

    Ok(Parameter { name, type_ })
}

fn parse_fn_params<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
) -> Result<Vec<Parameter>, ParseError> {
    // Advance the parenthesis open
    ctx.advance_required_symbol(TokenId::ParenOpen)?;
    let mut params: Vec<Parameter> = vec![];

    // Loop until a closing parenthesis has been found e.g:
    // ()
    // (a: int)
    // (a: int, b: char)
    // (a: int, b: char,)
    loop {
        if ctx.advance_optional(is_token_symbol(TokenId::ParenClose)) {
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

fn parse_block<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
) -> Result<Vec<Node>, ParseError> {
    ctx.advance_required_symbol(TokenId::BraceOpen)?;
    let mut lines = vec![];

    loop {
        if ctx.advance_optional(is_token_symbol(TokenId::BraceClose)) {
            break;
        }

        let line = parse_statement(ctx)?;
        lines.push(line);
    }

    Ok(lines)
}

fn parse_fn<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>) -> Result<Node, ParseError> {
    ctx.advance_required_symbol(TokenId::Fn)?;

    let identifier = parse_identifier(ctx)?;

    let parameters = parse_fn_params(ctx)?;

    ctx.advance_required_symbol(TokenId::Colon)?;
    let return_type = parse_type(ctx)?;

    let body = parse_block(ctx)?;

    let fn_def = FunctionDefinition {
        identifier,
        return_type,
        parameters,
        body,
    };
    Ok(Node::FunctionDefinition(fn_def))
}

fn parse_if_statement<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
    symbol: TokenId,
) -> Result<Node, ParseError> {
    ctx.advance_required_symbol(symbol)?;

    let expression = parse_expression(ctx)?;
    let true_branch = parse_block(ctx)?;

    let next = ctx.peek();
    let false_branch = match next.id {
        TokenId::ElIf => Ok(vec![parse_if_statement(ctx, TokenId::ElIf)?]),
        TokenId::Else => {
            ctx.advance_required_symbol(TokenId::Else)?;
            parse_block(ctx)
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

fn parse_unary_operation<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
    operation: UnaryOperation,
) -> Result<Node, ParseError> {
    ctx.advance();

    let operand_node = parse_expression(ctx)?;
    let operand = Box::new(operand_node);

    let expr = UnaryExpression { operation, operand };
    Ok(Node::UnaryExpression(expr))
}

fn parse_constant<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
) -> Result<Node, ParseError> {
    let token_value = parse_number(ctx)?;
    let number = token_value.parse::<i64>();

    match number {
        Ok(val) => Ok(Node::Constant(Constant {
            value: ConstantValue::Int64(val),
        })),
        Err(_) => Err(ParseError::UnknownConstant(token_value)),
    }
}

fn parse_primary<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>) -> Result<Node, ParseError> {
    let token = ctx.peek();
    match token.id {
        TokenId::Number(_) => parse_constant(ctx),
        _ => Err(ParseError::UnexpectedToken(token.clone())),
    }
}

fn parse_expression_bp<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
    min_bp: u8,
) -> Result<Node, ParseError> {
    let mut lhs = parse_primary(ctx)?;

    loop {
        // DO NOT CONSUME SEMICOLON
        // The end-of-expression token is consumed elsewhere
        // as all recursive calls need to end when a SemiColon is encountered
        // ------------------
        // Expressions are also valid within an if statement
        // Then the expression ends when a brace is encountered
        if ctx.expect(is_token_symbol(TokenId::SemiColon))
            || ctx.expect(is_token_symbol(TokenId::BraceOpen))
        {
            break;
        }

        let token = ctx.peek();
        let op = BinaryOperation::from_token_id(&token.id)?;

        let (l_bp, r_bp) = op.binding_power();

        // If l_bp < min_bp then we should exit and return the primary
        // because the previous operator has precendence over the current
        if l_bp < min_bp {
            break;
        }

        ctx.advance();
        let rhs = parse_expression_bp(ctx, r_bp)?;

        lhs = Node::BinaryExpression(BinaryExpression {
            operation: op,
            left: Box::new(lhs),
            right: Box::new(rhs),
        })
    }

    Ok(lhs)
}

fn parse_expression<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
) -> Result<Node, ParseError> {
    // Perform Pratt parsing https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    parse_expression_bp(ctx, 0)
}

fn parse_statement<I: Iterator<Item = Token>>(
    ctx: &mut ParseContext<I>,
) -> Result<Node, ParseError> {
    let token = ctx.peek();

    let (node, needs_semicolon) = match token.id {
        TokenId::Return => (parse_unary_operation(ctx, UnaryOperation::Return), true),
        TokenId::Assert => (parse_unary_operation(ctx, UnaryOperation::Assert), true),
        TokenId::If => (parse_if_statement(ctx, TokenId::If), false),
        _ => (Err(ParseError::UnexpectedToken(token.clone())), false),
    };

    if needs_semicolon {
        // Consume SemiColon if statement needs to be closed with one
        ctx.advance_required(is_token_symbol(TokenId::SemiColon))?;
    }

    node
}

fn parse_next<I: Iterator<Item = Token>>(ctx: &mut ParseContext<I>) -> Result<Node, ParseError> {
    let token = ctx.peek();

    let node = match token.id {
        TokenId::Fn => parse_fn(ctx)?,
        _ => return Err(ParseError::UnexpectedToken(token.clone())),
    };

    Ok(node)
}

pub fn parse(lexer: Lexer) -> Result<Node, ParseError> {
    let mut ctx = ParseContext::new(lexer);
    parse_next(&mut ctx)
}

impl<I: Iterator<Item = Token>> ParseContext<I> {
    pub fn new(tokens: I) -> Self {
        ParseContext {
            tokens: tokens.peekable(),
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
            Ok(self.advance())
        } else {
            let s = self.peek();
            Err(ParseError::UnexpectedToken(s.clone()))
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
            let _ = self.advance();
        }
        result
    }

    fn advance_required_symbol(&mut self, id: TokenId) -> Result<Token, ParseError> {
        self.advance_required(is_token_symbol(id))
    }
}
