# Todo

- Lexer
    - Error handling for token streaming
    - Refactor Lexer to work with stream rather than entire string
- AST parser
    - More robust AST error handling
        - Return a ParseResult rather than a Node
        - Actually consider when to synchronize to what tokens
    - Split up to module
    - Type verification for AST -> TypedAST
    - Semantic verification -> e.g. function must provide return statement
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
    - Call program from bonobo standard library
    - argc and argv
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
