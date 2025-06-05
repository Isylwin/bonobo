#![allow(dead_code)]

use std::fmt;

#[macro_export]
macro_rules! xor {
    ($reg:ident) => {
        AsmInstruction::Xor($reg, $reg)
    };
}

#[derive(Debug, Clone)]
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
    Nop,
    FnCall(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmOperand {
    Register(AsmRegister),
    Immediate(i64),
    Label(String),
    Memory(String),
    StackBase(i64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

pub const RAX: AsmOperand = AsmOperand::Register(AsmRegister::Rax); // Function return & syscall
pub const RBX: AsmOperand = AsmOperand::Register(AsmRegister::Rbx); // preserved
pub const RDI: AsmOperand = AsmOperand::Register(AsmRegister::Rdi); // Function arg #1
pub const RSI: AsmOperand = AsmOperand::Register(AsmRegister::Rsi); // Function arg #2
pub const RDX: AsmOperand = AsmOperand::Register(AsmRegister::Rdx); // Function arg #3
pub const RCX: AsmOperand = AsmOperand::Register(AsmRegister::Rcx); // Function arg #4
pub const RBP: AsmOperand = AsmOperand::Register(AsmRegister::Rbp); // Base stack pointer
pub const RSP: AsmOperand = AsmOperand::Register(AsmRegister::Rsp); // Stack pointer
pub const DIL: AsmOperand = AsmOperand::Register(AsmRegister::Dil); // RDI lower byte
pub const AL: AsmOperand = AsmOperand::Register(AsmRegister::Al); // RAX lower byte

pub const FALSE: AsmOperand = AsmOperand::Immediate(0);
pub const TRUE: AsmOperand = AsmOperand::Immediate(1);

// System V AMD64 ABI calling convention defines these registers
// as the order for the first 4 arguments to a function
pub const FN_ARG_REG: [AsmOperand; 4] = [RDI, RSI, RDX, RCX];

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
