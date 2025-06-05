use std::io::Write;

use crate::bonobo::{
    asm::{Emit, emitter::EmitError, parser::parse_ast_program},
    ast::AstProgram,
};

pub fn emit_asm_code(ast_program: AstProgram, writer: &mut dyn Write) -> Result<(), EmitError> {
    let program = parse_ast_program(&ast_program)?;
    program.emit(writer)
}
