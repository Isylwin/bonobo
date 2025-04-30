alias r := run
alias b := build
alias v := verify

# Usage: just build file_name
# Builds from: example/<file_name>.bnb
# Outputs: out/<file_name>.asm, .o, and binary
build file_name:
  mkdir -p out
  cargo run -- examples/{{file_name}}.bnb -o out/{{file_name}}.asm
  nasm -felf64 out/{{file_name}}.asm -o out/{{file_name}}.o
  ld out/{{file_name}}.o -o out/{{file_name}}

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

# Builds the example/return_0.bnb program
# Output executable will be out/return_0
return_42:
  just build return_42
  just run return_42

verify:
  cargo fmt
  cargo test
  cargo check
  cargo clippy

fix:
  cargo clippy --fix --bin "bonobo"

list-examples:
  ls example/*.bnb | sed 's|example/||; s|\.bnb||'
