# Todo

- Bonobo program which tests its own language
- Error handling for token streaming
- Error handling for AST parsing
- Error handling for Compiler
- Refactor Lexer to work with stream rather than entire string
- Extend grammer with following constructs:
    - if - else
    - while loop
    - function calls
    - basic arithmetic (add, sub, mult, modulo)
    - variable assignment
    - compile time const strings
- Type verification for AST -> TypedAST
- Command line arguments for Bonobo programs
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
