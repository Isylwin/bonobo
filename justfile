alias r := run
alias b := build
alias v := verify

build-std:
  mkdir -p out/std
  nasm -f elf64 -g -F dwarf std/assert.asm -o out/std/__assert.o

# Usage: just build file_name
# Builds from: example/<file_name>.bnb
# Outputs: out/<file_name>.asm, .o, and binary
build file_name: build-std
  mkdir -p out
  cargo run -- examples/{{file_name}}.bnb -o out/{{file_name}}.asm
  nasm -f elf64 -g -F dwarf -o out/{{file_name}}.o out/{{file_name}}.asm
  ld out/{{file_name}}.o out/std/__assert.o -o out/{{file_name}}

# Runs any file in the out directory
run file_name:
  out/{{file_name}}

# Cleans the output directory
clean:
  rm -rf out/

# Builds the example/return_0.bnb program
# Output executable will be out/return_0
return_0:
  just build return_0
  just run return_0

# Builds the example/return_42.bnb program
# Output executable will be out/return_42
return_42:
  just build return_42
  just run return_42

# Builds the example/assert.bnb program
# Output executable will be out/assert
assert:
  just build assert
  just run assert

# Builds the example/expressions.bnb program
# Output executable will be out/expressions
expressions:
  just build expressions
  just run expressions

# Builds the example/ifthen.bnb program
# Output executable will be out/ifthen
ifthen:
  just build ifthen
  just run ifthen

run-all:
  just return_0
  just assert
  just expressions
  just ifthen

verify:
  cargo fmt
  cargo test
  cargo check
  cargo clippy

fix:
  cargo clippy --fix --bin "bonobo"

list-examples:
  ls example/*.bnb | sed 's|example/||; s|\.bnb||'
