use std::{io::Write, iter::zip};

use crate::{
    bonobo::{
        asm::{
            AsmInstruction, AsmOperand, AsmProgram, Emit,
            emitter::EmitError,
            operand::{AL, FN_ARG_REG, RAX, RBP, RCX, RDI, RDX, RSP, TRUE},
        },
        ast::{AstProgram, FunctionCall},
    },
    xor,
};

use crate::bonobo::ast::{
    BinaryExpression, BinaryOperation, Constant, ConstantValue, FunctionDefinition, Identifier,
    IfStatement, Node, UnaryExpression, UnaryOperation, VariableAssignment, VariableDeclaration,
};

#[derive(Debug)]
pub enum AsmParseError {
    UnknownAstNode,
    UndeclaredVariable,
    TooManyArguments,
}

#[derive(Debug)]
struct AsmParser {
    program: AstProgram,
}

impl AsmParser {
    fn new(program: AstProgram) -> Self {
        AsmParser { program }
    }

    fn parse_function(
        &self,
        mut program: AsmProgram,
        func: &FunctionDefinition,
    ) -> Result<AsmProgram, AsmParseError> {
        // Throw error if parameters exceed max supported length
        if func.parameters.len() > 4 {
            return Err(AsmParseError::TooManyArguments);
        }

        program.reset_env();

        let label = AsmOperand::Label(func.identifier.clone());
        program.add_instruction(AsmInstruction::Label(label));

        // Push RBP to stack and set to RSP
        program.add_instruction(AsmInstruction::Push(RBP));
        program.add_instruction(AsmInstruction::Move(RSP, RBP));
        // Reserve 32 bytes for stack
        program.add_instruction(AsmInstruction::Subtract(AsmOperand::Immediate(32), RBP));

        // --- Parse parameters ---
        let param_zip = zip(&func.parameters, FN_ARG_REG);
        for (param, src) in param_zip {
            // Declare the param as a local variable and store it using the RBP
            program.env.declare_local(&param.name);

            let offset = program
                .env
                .get_local(&param.name)
                .ok_or(AsmParseError::UndeclaredVariable)?;

            program.add_instruction(AsmInstruction::Move(src, AsmOperand::StackBase(offset)));
        }

        // --- Parse body ---
        for node in &func.body {
            program = self.parse_node(program, node)?;
        }

        // See return implementation for epilogue

        Ok(program)
    }

    fn parse_if_statement(
        &self,
        program: AsmProgram,
        if_stmt: &IfStatement,
    ) -> Result<AsmProgram, AsmParseError> {
        let mut prog = self.parse_node(program, if_stmt.expression.as_ref())?;

        let true_label = prog.gen_label("if_t_");
        let end_label = prog.gen_label("if_e_");

        prog.add_instruction(AsmInstruction::Pop(RAX));
        prog.add_instruction(AsmInstruction::Compare(TRUE, RAX));
        prog.add_instruction(AsmInstruction::JumpEqual(true_label.clone()));
        for node in &if_stmt.false_branch {
            prog = self.parse_node(prog, node)?;
        }
        prog.add_instruction(AsmInstruction::Jump(end_label.clone()));
        prog.add_instruction(AsmInstruction::Label(true_label));
        for node in &if_stmt.true_branch {
            prog = self.parse_node(prog, node)?;
        }
        prog.add_instruction(AsmInstruction::Label(end_label));

        Ok(prog)
    }

    fn parse_return(&self, program: AsmProgram, inner: &Node) -> Result<AsmProgram, AsmParseError> {
        let mut prog = self.parse_node(program, inner)?;

        // Pop into RAX from stack -- assumes we are returning a value
        prog.add_instruction(AsmInstruction::Pop(RAX));

        // Pop saved RBP from stack
        prog.add_instruction(AsmInstruction::Pop(RBP));

        // Return from function
        prog.add_instruction(AsmInstruction::Return);
        Ok(prog)
    }

    fn parse_assert(&self, program: AsmProgram, inner: &Node) -> Result<AsmProgram, AsmParseError> {
        let mut prog = self.parse_node(program, inner)?;

        prog.add_extern("assert");
        prog.add_instruction(AsmInstruction::Pop(RDI));
        prog.add_instruction(AsmInstruction::FnCall("assert".into()));
        Ok(prog)
    }

    fn parse_declaration(
        &self,
        mut program: AsmProgram,
        identifier: &str,
    ) -> Result<AsmProgram, AsmParseError> {
        program.env.declare_local(identifier);
        Ok(program)
    }

    fn parse_const_int(
        &self,
        mut program: AsmProgram,
        value: i64,
    ) -> Result<AsmProgram, AsmParseError> {
        program.add_instruction(AsmInstruction::Move(AsmOperand::Immediate(value), RAX));
        program.add_instruction(AsmInstruction::Push(RAX));
        Ok(program)
    }

    fn parse_identifier(
        &self,
        mut program: AsmProgram,
        identifier: &str,
    ) -> Result<AsmProgram, AsmParseError> {
        let offset = program
            .env
            .get_local(identifier)
            .ok_or(AsmParseError::UndeclaredVariable)?;

        program.add_instruction(AsmInstruction::Move(AsmOperand::StackBase(offset), RAX));
        program.add_instruction(AsmInstruction::Push(RAX));

        Ok(program)
    }

    fn parse_binary_expression(
        &self,
        mut program: AsmProgram,
        op: &BinaryOperation,
        lhs: &Node,
        rhs: &Node,
    ) -> Result<AsmProgram, AsmParseError> {
        // Output of binary expression will be stored on stack

        // First generate the right hand side
        // Some operations, such as substraction are not associative
        // The RHS needs to be substracted from the LHS
        // and the result should end up in the RDI register

        program = self.parse_node(program, lhs)?;
        program = self.parse_node(program, rhs)?;

        // LHS = 2nd on stack
        // RHS = 1st on stack

        match op {
            BinaryOperation::Add => {
                // Addition is associative
                program.add_instruction(AsmInstruction::Pop(RCX));
                program.add_instruction(AsmInstruction::Pop(RDI));
                program.add_instruction(AsmInstruction::Add(RCX, RDI));
                program.add_instruction(AsmInstruction::Push(RDI));
            }
            BinaryOperation::Subtract => {
                program.add_instruction(AsmInstruction::Pop(RCX));
                program.add_instruction(AsmInstruction::Pop(RDI));
                program.add_instruction(AsmInstruction::Subtract(RCX, RDI));
                program.add_instruction(AsmInstruction::Push(RDI));
            }
            BinaryOperation::Multiply => {
                // Multiplication is associative
                program.add_instruction(AsmInstruction::Pop(RCX));
                program.add_instruction(AsmInstruction::Pop(RDI));
                program.add_instruction(AsmInstruction::SignedMultiply(RCX, RDI));
                program.add_instruction(AsmInstruction::Push(RDI));
            }
            BinaryOperation::Modulo => {
                // https://en.wikibooks.org/wiki/X86_Assembly/Arithmetic
                // Dividend is stored in RAX + RDX (lower bits in RAX)
                // CQO or CQD needs to be used to sign extend RAX
                // Remainder stored in RDX
                // Quotient stored in RAX
                program.add_instruction(AsmInstruction::Pop(RCX));
                program.add_instruction(AsmInstruction::Pop(RAX));
                program.add_instruction(AsmInstruction::Cqo);
                program.add_instruction(AsmInstruction::SignedDivide(RCX));
                program.add_instruction(AsmInstruction::Push(RDX));
            }
            BinaryOperation::Division => {
                // See Modulo
                program.add_instruction(AsmInstruction::Pop(RCX));
                program.add_instruction(AsmInstruction::Pop(RAX));
                program.add_instruction(AsmInstruction::Cqo);
                program.add_instruction(AsmInstruction::SignedDivide(RCX));
                program.add_instruction(AsmInstruction::Push(RAX));
            }
            BinaryOperation::Equals => {
                program.add_instruction(AsmInstruction::Pop(RCX));
                program.add_instruction(AsmInstruction::Pop(RDI));
                program.add_instruction(xor!(RAX));
                program.add_instruction(AsmInstruction::Compare(RCX, RDI));
                program.add_instruction(AsmInstruction::SetEqual(AL));
                program.add_instruction(AsmInstruction::Push(RAX));
            }
        }
        Ok(program)
    }

    fn parse_assignment(
        &self,
        mut program: AsmProgram,
        identifier: &str,
        value: &Node,
    ) -> Result<AsmProgram, AsmParseError> {
        program = self.parse_node(program, value)?;
        let offset = program
            .env
            .get_local(identifier)
            .ok_or(AsmParseError::UndeclaredVariable)?;

        program.add_instruction(AsmInstruction::Pop(RAX));
        program.add_instruction(AsmInstruction::Move(RAX, AsmOperand::StackBase(offset)));

        Ok(program)
    }

    fn parse_function_call(
        &self,
        mut program: AsmProgram,
        fn_call: &FunctionCall,
    ) -> Result<AsmProgram, AsmParseError> {
        // Only support 4 args for now
        if fn_call.arguments.len() > 4 {
            return Err(AsmParseError::TooManyArguments);
        }

        // Create zipped iterator over the argument registers and the arguments
        let arg_zip = zip(&fn_call.arguments, FN_ARG_REG);

        // MAYBE Check for existence of function

        // Throw relevant args in their respective registers
        for (arg, dst) in arg_zip {
            program = self.parse_node(program, arg)?;
            program.add_instruction(AsmInstruction::Pop(dst));
        }

        // Call function
        program.add_instruction(AsmInstruction::FnCall(fn_call.identifier.clone()));

        // Push result onto stack
        program.add_instruction(AsmInstruction::Push(RAX));

        Ok(program)
    }

    fn parse_node(&self, program: AsmProgram, node: &Node) -> Result<AsmProgram, AsmParseError> {
        let result = match node {
            Node::FunctionDefinition(func) => self.parse_function(program, func),
            Node::UnaryExpression(UnaryExpression {
                operation: UnaryOperation::Return,
                operand: inner,
            }) => self.parse_return(program, inner.as_ref()),
            Node::UnaryExpression(UnaryExpression {
                operation: UnaryOperation::Assert,
                operand: inner,
            }) => self.parse_assert(program, inner.as_ref()),
            Node::VariableDeclaration(VariableDeclaration {
                identifier,
                type_: _,
            }) => self.parse_declaration(program, identifier),
            Node::VariableAssignment(VariableAssignment { identifier, value }) => {
                self.parse_assignment(program, identifier, value)
            }
            Node::Constant(Constant {
                value: ConstantValue::Int64(val),
            }) => self.parse_const_int(program, *val),
            Node::Identifier(Identifier { name }) => self.parse_identifier(program, name),
            Node::BinaryExpression(BinaryExpression {
                left: lhs,
                right: rhs,
                operation: op,
            }) => self.parse_binary_expression(program, op, lhs.as_ref(), rhs.as_ref()),
            Node::IfStatement(if_stmt) => self.parse_if_statement(program, if_stmt),
            Node::FunctionCall(fn_call) => self.parse_function_call(program, fn_call),
            Node::Error => Err(AsmParseError::UnknownAstNode),
        };

        result
    }

    fn parse(&self) -> Result<AsmProgram, AsmParseError> {
        let mut program = AsmProgram::new();
        for node in &self.program.functions {
            program = self.parse_node(program, node)?;
        }
        Ok(program)
    }
}

pub fn emit(ast_program: AstProgram, writer: &mut dyn Write) -> Result<(), EmitError> {
    let parser = AsmParser::new(ast_program);
    let program = parser.parse();

    program?.emit(writer)
}
