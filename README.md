# Todo

- Lexer
    - Error handling for token streaming
    - Refactor Lexer to work with stream rather than entire string
- AST parser
    - More robust AST error handling
        - Return a ParseResult rather than a Node
        - Actually consider when to synchronize to what tokens
    - Type verification for AST -> TypedAST
    - Semantic verification -> e.g. function must provide return statement
- Assembly frontend
    - Split up to module
    - Write optimizer for assembly frontend
    - Fix booboo doodoo 32 bytes of reserved space for local stack
    - Allow for more than 4 args to a function call
- Grammar
    - while loop
    - more comparison operations
    - more assignment operations
    - Add parenthesised expressions
    - compile time const strings
- Standard library
    - Write more std functions for Bonobo
        - print to output and read from input
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

The generated assembly code adheres to the System V AMD64 ABI calling convention,
as the supported operating system is Linux.

## Other stuff

### Operator precendence

| Precedence | Operator     | Description                                       | Associativity |
|------------|--------------|---------------------------------------------------|---------------|
|    29,30   | ++ --        | Suffix/postfix increment and decrement            | Left-to-right |
|            | ()           | Function call                                     |               |
|            | []           | Array subscripting                                |               |
|            | .            | Structure and union member access                 |               |
|            | ->           | Structure and union member access through pointer |               |
|            | (type){list} | Compound literal(C99)                             |               |
|    27,28   | ++ --        | Prefix increment and decrement[note 1]            | Right-to-left |
|            | + -          | Unary plus and minus                              |               |
|            | ! ~          | Logical NOT and bitwise NOT                       |               |
|            | (type)       | Cast                                              |               |
|            | *            | Indirection (dereference)                         |               |
|            | &            | Address-of                                        |               |
|            | sizeof       | Size-of[note 2]                                   |               |
|            | _Alignof     | Alignment requirement(C11)                        |               |
|    25,26   | * / %        | Multiplication, division, and remainder           | Left-to-right |
|    23,24   | + -          | Addition and subtraction                          |               |
|    21,22   | << >>        | Bitwise left shift and right shift                |               |
|    19,20   | < <=         | For relational operators < and ≤ respectively     |               |
|            | > >=         | For relational operators > and ≥ respectively     |               |
|    17,18   | == !=        | For relational = and ≠ respectively               |               |
|    15,16   | &            | Bitwise AND                                       |               |
|    13,14   | ^            | Bitwise XOR (exclusive or)                        |               |
|    11,12   | \|           | Bitwise OR (inclusive or)                         |               |
|     9,10   | &&           | Logical AND                                       |               |
|     7,8    | \|\|         | Logical OR                                        |               |
|     5,6    | ?:           | Ternary conditional[note 3]                       | Right-to-left |
|     3,4    | =            | Simple assignment                                 |               |
|            | += -=        | Assignment by sum and difference                  |               |
|            | *= /= %=     | Assignment by product, quotient, and remainder    |               |
|            | <<= >>=      | Assignment by bitwise left shift and right shift  |               |
|            | &= ^= \|=    | Assignment by bitwise AND, XOR, and OR            |               |
|     1,2    | ,            | Comma                                             | Left-to-right |

