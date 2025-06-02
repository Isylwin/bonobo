#![allow(dead_code)]

use std::{
    collections::{HashMap, HashSet},
    fmt,
    io::{self, Write},
};

use super::ast::{
    BinaryExpression, BinaryOperation, Constant, ConstantValue, FunctionDefinition, Identifier,
    IfStatement, Node, UnaryExpression, UnaryOperation, VariableAssignment, VariableDeclaration,
};

#[derive(Debug)]
struct AsmProgram {
    externs: HashSet<String>,
    globals: Vec<String>,
    text: AsmSection,
    data: AsmSection,
    env: AsmEnvironment,
}

#[derive(Debug)]
struct AsmEnvironment {
    labels: HashMap<String, i64>,
    local_offsets: HashMap<String, i64>,
    next_local_offset: i64,
}

#[derive(Debug)]
struct AsmSection {
    name: String,
    instructions: Vec<AsmInstruction>,
}

#[derive(Debug)]
pub enum AsmInstruction {
    Label(AsmOperand),                      // Should always be a label
    Move(AsmOperand, AsmOperand),           // src, dst
    Swap(AsmOperand, AsmOperand),           // src, dst
    MoveEqual(AsmOperand, AsmOperand),      // src, dst
    MoveNotEqual(AsmOperand, AsmOperand),   // src, dst
    Push(AsmOperand),                       // src
    Pop(AsmOperand),                        // dst
    SetEqual(AsmOperand),                   // dst
    Compare(AsmOperand, AsmOperand),        // src, dst
    Jump(AsmOperand),                       // loc
    JumpEqual(AsmOperand),                  // loc
    Add(AsmOperand, AsmOperand),            // src, dst
    Subtract(AsmOperand, AsmOperand),       // src, dst
    SignedMultiply(AsmOperand, AsmOperand), // src, dst
    SignedDivide(AsmOperand),               // divisor
    Xor(AsmOperand, AsmOperand),            // src, dst
    Cqo,
    Return,
    Syscall,
    FnCall(String),
}

#[derive(Debug, Clone)]
pub enum AsmOperand {
    Register(AsmRegister),
    Immediate(i64),
    Label(String),
    Memory(String),
    StackBase(i64),
}

const RAX: AsmOperand = AsmOperand::Register(AsmRegister::Rax); // Function return & syscall
const RBX: AsmOperand = AsmOperand::Register(AsmRegister::Rbx); // preserved
const RCX: AsmOperand = AsmOperand::Register(AsmRegister::Rcx); // Operand 1
const RDX: AsmOperand = AsmOperand::Register(AsmRegister::Rdx); // Operand 2
const RDI: AsmOperand = AsmOperand::Register(AsmRegister::Rdi); // Function arg #1
const RBP: AsmOperand = AsmOperand::Register(AsmRegister::Rbp); // Base stack pointer
const RSP: AsmOperand = AsmOperand::Register(AsmRegister::Rsp); // Stack pointer
const DIL: AsmOperand = AsmOperand::Register(AsmRegister::Dil); // RDI lower byte
const AL: AsmOperand = AsmOperand::Register(AsmRegister::Al); // RAX lower byte

const FALSE: AsmOperand = AsmOperand::Immediate(0);
const TRUE: AsmOperand = AsmOperand::Immediate(1);

macro_rules! xor {
    ($reg:ident) => {
        AsmInstruction::Xor($reg, $reg)
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmRegister {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rdi,
    Rsi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    Dil,
    Al,
}

impl fmt::Display for AsmRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            AsmRegister::Rax => "rax",
            AsmRegister::Rbx => "rbx",
            AsmRegister::Rcx => "rcx",
            AsmRegister::Rdx => "rdx",
            AsmRegister::Rsi => "rsi",
            AsmRegister::Rdi => "rdi",
            AsmRegister::Rbp => "rbp",
            AsmRegister::Rsp => "rsp",
            AsmRegister::R8 => "r8",
            AsmRegister::R9 => "r9",
            AsmRegister::R10 => "r10",
            AsmRegister::R11 => "r11",
            AsmRegister::R12 => "r12",
            AsmRegister::R13 => "r13",
            AsmRegister::R14 => "r14",
            AsmRegister::R15 => "r15",
            AsmRegister::Dil => "dil",
            AsmRegister::Al => "al",
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for AsmOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmOperand::Register(s) => write!(f, "{}", s),
            AsmOperand::Immediate(s) => write!(f, "{}", s),
            AsmOperand::Label(s) => write!(f, "{}", s),
            AsmOperand::Memory(s) => write!(f, "{}", s),
            AsmOperand::StackBase(offset) => write!(f, "QWORD [rbp{}]", offset),
        }
    }
}

impl AsmEnvironment {
    fn new() -> Self {
        let labels = HashMap::new();
        let local_offsets = HashMap::new();

        AsmEnvironment {
            labels,
            local_offsets,
            next_local_offset: 0,
        }
    }

    fn next_label(&mut self, prefix: &str) -> AsmOperand {
        let label = match self.labels.get_mut(prefix) {
            Some(v) => {
                *v += 1;
                format!("{}{}", prefix, v)
            }
            None => {
                self.labels.insert(prefix.into(), 0);
                format!("{}0", prefix)
            }
        };
        AsmOperand::Label(label)
    }

    fn declare_local(&mut self, identifier: &str) {
        self.local_offsets
            .insert(identifier.into(), self.next_local_offset - 8);
        self.next_local_offset -= 8;
    }

    fn get_local(&self, identifier: &str) -> Option<i64> {
        self.local_offsets.get(identifier).copied()
    }
}

impl AsmProgram {
    fn new() -> Self {
        // Assume that the user's program always defines the banana entrypoint
        let globals = vec!["banana".into()];
        let externs = HashSet::new();
        let text = AsmSection {
            name: ".text".into(),
            instructions: vec![],
        };
        let data = AsmSection {
            name: ".data".into(),
            instructions: vec![],
        };
        let env = AsmEnvironment::new();

        AsmProgram {
            externs,
            globals,
            text,
            data,
            env,
        }
    }

    fn add_instruction(&mut self, instruction: AsmInstruction) {
        self.text.instructions.push(instruction)
    }

    fn add_extern(&mut self, external: &str) {
        self.externs.insert(external.into());
    }

    fn gen_label(&mut self, prefix: &str) -> AsmOperand {
        self.env.next_label(prefix)
    }
}

#[derive(Debug)]
pub enum EmitError {
    UnknownInstruction(AsmInstruction),
    Io(io::Error),
    AsmParseError(AsmParseError),
}

impl From<io::Error> for EmitError {
    fn from(src: io::Error) -> EmitError {
        EmitError::Io(src)
    }
}

impl From<AsmParseError> for EmitError {
    fn from(src: AsmParseError) -> EmitError {
        EmitError::AsmParseError(src)
    }
}

trait Emit {
    fn emit(&self, writer: &mut dyn Write) -> Result<(), EmitError>;
}

impl Emit for AsmInstruction {
    fn emit(&self, writer: &mut dyn Write) -> Result<(), EmitError> {
        match self {
            AsmInstruction::Label(s) => writeln!(writer, "{}:", s)?,
            AsmInstruction::Move(src, dst) => {
                writeln!(writer, "\t\tmov\t\t{}, {}", dst, src)?;
            }
            AsmInstruction::Swap(src, dst) => {
                writeln!(writer, "\t\txchg\t{}, {}", dst, src)?;
            }
            AsmInstruction::MoveEqual(src, dst) => {
                writeln!(writer, "\t\tcmove\t{}, {}", dst, src)?;
            }
            AsmInstruction::MoveNotEqual(src, dst) => {
                writeln!(writer, "\t\tcmovne\t{}, {}", dst, src)?;
            }
            AsmInstruction::Push(src) => {
                writeln!(writer, "\t\tpush\t{}", src)?;
            }
            AsmInstruction::Pop(dst) => {
                writeln!(writer, "\t\tpop\t\t{}", dst)?;
            }
            AsmInstruction::SetEqual(dst) => {
                writeln!(writer, "\t\tsete\t{}", dst)?;
            }
            AsmInstruction::Compare(src, dst) => {
                writeln!(writer, "\t\tcmp\t\t{}, {}", dst, src)?;
            }
            AsmInstruction::Jump(loc) => {
                writeln!(writer, "\t\tjmp\t\t{}", loc)?;
            }
            AsmInstruction::JumpEqual(loc) => {
                writeln!(writer, "\t\tje\t\t{}", loc)?;
            }
            AsmInstruction::Add(src, dst) => {
                writeln!(writer, "\t\tadd\t\t{}, {}", dst, src)?;
            }
            AsmInstruction::Subtract(src, dst) => {
                writeln!(writer, "\t\tsub\t\t{}, {}", dst, src)?;
            }
            AsmInstruction::SignedMultiply(src, dst) => {
                writeln!(writer, "\t\timul\t{}, {}", dst, src)?;
            }
            AsmInstruction::SignedDivide(divisor) => {
                writeln!(writer, "\t\tidiv\t{}", divisor)?;
            }
            AsmInstruction::Xor(src, dst) => {
                writeln!(writer, "\t\txor\t\t{}, {}", dst, src)?;
            }
            AsmInstruction::Cqo => writeln!(writer, "\t\tcqo")?,
            AsmInstruction::Syscall => writeln!(writer, "\t\tsyscall")?,
            AsmInstruction::Return => writeln!(writer, "\t\tret")?,
            AsmInstruction::FnCall(s) => writeln!(writer, "\t\tcall\t{}", s)?,
        }
        Ok(())
    }
}

impl Emit for AsmOperand {
    fn emit(&self, writer: &mut dyn Write) -> Result<(), EmitError> {
        write!(writer, "{}", self)?;
        Ok(())
    }
}

impl Emit for AsmSection {
    fn emit(&self, writer: &mut dyn Write) -> Result<(), EmitError> {
        writeln!(writer, "\t\tsection {}", self.name)?;
        for instruction in &self.instructions {
            instruction.emit(writer)?;
        }
        Ok(())
    }
}

impl Emit for AsmProgram {
    fn emit(&self, writer: &mut dyn Write) -> Result<(), EmitError> {
        write!(
            writer,
            ";Auto-generated by Bonobo ASM generator\n;  _____\n;o( . . )o\n; __(-)__\n\n"
        )?;
        for var_name in &self.globals {
            writeln!(writer, "\t\tglobal {}", var_name)?;
        }
        writeln!(writer)?;
        for var_name in &self.externs {
            writeln!(writer, "\t\textern {}", var_name)?;
        }
        writeln!(writer)?;
        self.text.emit(writer)?;
        writeln!(writer)?;
        self.data.emit(writer)?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum AsmParseError {
    UnknownAstNode,
    UndeclaredVariable,
}

#[derive(Debug)]
struct AsmParser {
    root: Node,
}

impl AsmParser {
    fn new(root: Node) -> Self {
        AsmParser { root }
    }

    fn parse_function(
        &self,
        mut program: AsmProgram,
        func: &FunctionDefinition,
    ) -> Result<AsmProgram, AsmParseError> {
        let label = AsmOperand::Label(func.identifier.clone());
        program.add_instruction(AsmInstruction::Label(label));

        // Push RBP to stack and set to RSP
        program.add_instruction(AsmInstruction::Push(RBP));
        program.add_instruction(AsmInstruction::Move(RSP, RBP));
        // Reserve 24 bytes for stack
        program.add_instruction(AsmInstruction::Subtract(AsmOperand::Immediate(24), RBP));

        //Disregard parameters for now

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
            _ => Err(AsmParseError::UnknownAstNode),
        };

        result
    }

    fn parse(&self) -> Result<AsmProgram, AsmParseError> {
        let program = AsmProgram::new();
        self.parse_node(program, &self.root)
    }
}

pub fn emit(root: Node, writer: &mut dyn Write) -> Result<(), EmitError> {
    let parser = AsmParser::new(root);
    let program = parser.parse();

    program?.emit(writer)
}
