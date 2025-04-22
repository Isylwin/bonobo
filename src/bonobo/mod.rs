use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

use ast::Parser;
use lexer::Lexer;

mod ast;
mod lexer;

pub fn bonobo_compile(file_name: &str) {
    let src = read_file(file_name);
    let lexer = Lexer::new(&src);
    for token in Lexer::new(&src) {
        println!("{}", token);
    }

    let mut parser = Parser::new(lexer);
    let result = parser.parse();
    println!("{:?}", result);
}

fn read_file(file_name: &str) -> String {
    // Create a path to the desired file
    let path = Path::new(file_name);
    let display = path.display();

    // Open the path in read-only mode, returns `io::Result<File>`
    let mut file = match File::open(path) {
        Err(why) => panic!("couldn't open {}: {}", display, why),
        Ok(file) => file,
    };

    // Read the file contents into a string, returns `io::Result<usize>`
    let mut s = String::new();
    if let Err(why) = file.read_to_string(&mut s) {
        panic!("couldn't read {}: {}", display, why)
    }
    s
}
