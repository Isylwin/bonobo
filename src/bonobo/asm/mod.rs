pub mod emitter;
pub mod main;
pub mod operand;
pub mod parser;
pub mod program;

pub use emitter::Emit;
pub use operand::{AsmInstruction, AsmOperand};
pub use program::AsmProgram;

// Re-export the main public API
pub use main::emit_asm_code;
