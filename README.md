# Todo

- Lexer
    - Error handling for token streaming
    - Refactor Lexer to work with stream rather than entire string
    - Add line number and char index to Token
    - Create EoF token
- AST parser
    - Change parsing structure to use a ParsingContext struct instead of &self
    - Error handling for AST parsing -> better error collection
        - Catch all errors in parsing context
    - Create "DSL" like language for parsing -> would be easier to split up
    - Type verification for AST -> TypedAST
    - Work with EoF token
- Assembly frontend
    - Split up to module
    - Write optimizer for assembly frontend
- Grammar
    - while loop
    - function calls
    - variable assignment
    - compile time const strings
- Standard library
    - Write more std functions for Bonobo
        - print to output and read from input
- General
    - Bonobo program which tests its own language
    - More intricate verbosity
    - More logging with increased verbosity
    - Allow stdin as input
    - Integrate nasm and ld calls into program

# Dependencies

- [NASM assembler](https://linuxtldr.com/installing-nasm/)
- [Rust](https://www.rust-lang.org/tools/install)
- [just](https://github.com/casey/just) for running utility commands

# Platform support

## Operating system

This compiler is exclusively developed to interface with Linux.
No attempt is made at this time to separate the assembly generation from Linux constraints.

## CPU architecture / assembler support

The targeted assembly language dialect is NASM.
This means that the only supported CPU architecture is x86 as NASM solely targets x86
An attempt might be made to support both NASM as well as GAS in the future.
