use std::fs::File;
use std::io::{BufReader, BufWriter, prelude::*};

use anyhow::Result;

use as_frontend::emit;
use ast::Parser;
use lexer::Lexer;

mod as_frontend;
mod ast;
mod lexer;

pub struct Compiler {
    is_verbose: bool,
    input_stream: BufReader<File>,
    output_stream: BufWriter<File>,
}

impl Compiler {
    pub fn new(
        is_verbose: bool,
        input_stream: BufReader<File>,
        output_stream: BufWriter<File>,
    ) -> Self {
        Compiler {
            is_verbose,
            input_stream,
            output_stream,
        }
    }

    pub fn compile(&mut self) -> Result<()> {
        let mut src = String::new();
        self.input_stream.read_to_string(&mut src)?;

        let lexer = Lexer::new(&src);
        if self.is_verbose {
            for token in Lexer::new(&src) {
                println!("{}", token);
            }
        }

        let mut parser = Parser::new(lexer);
        let result = parser.parse()?;

        if self.is_verbose {
            println!("{:?}", result);
        }

        emit(result, &mut self.output_stream).expect("Unable to write data");
        Ok(())
    }
}
