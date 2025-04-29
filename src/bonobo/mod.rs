use std::fs::File;
use std::io::{BufReader, BufWriter, prelude::*};

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

    pub fn compile(&mut self) {
        let mut src = String::new();
        let _ = self.input_stream.read_to_string(&mut src);

        let lexer = Lexer::new(&src);
        if self.is_verbose {
            for token in Lexer::new(&src) {
                println!("{}", token);
            }
        }

        let mut parser = Parser::new(lexer);
        let result = parser.parse();

        if self.is_verbose {
            println!("{:?}", result);
        }

        // let f = File::create("out/foo.s").expect("Unable to create file");
        // let mut f = BufWriter::new(f);
        emit(result.unwrap(), &mut self.output_stream).expect("Unable to write data");
    }
}
