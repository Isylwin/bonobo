use std::{
    fs::File,
    io::{BufReader, BufWriter},
    path::PathBuf,
};

use clap::Parser;

use bonobo::Compiler;

mod bonobo;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Sets the input file
    #[arg(value_name = "FILE")]
    input: PathBuf,

    /// Turn debugging information on
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbosity: u8,

    /// Sets the output file
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    let is_verbose = !matches!(cli.verbosity, 0);

    let input_stream = BufReader::new(File::open(cli.input)?);

    let output_file = match cli.output {
        Some(path) => File::create(path),
        _ => File::create("a.asm"),
    };

    let output_stream = BufWriter::new(output_file?);

    let mut compiler = Compiler::new(is_verbose, input_stream, output_stream);

    compiler.compile();
    Ok(())
}
