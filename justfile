# Builds the example/return_0.bnb program
# Output executable will be out/return_0
return_0:
  cargo run 
  nasm -felf64 out/foo.s 
  ld out/foo.o -o out/return_0

verify:
  cargo fmt
  cargo test
  cargo check
  cargo clippy
