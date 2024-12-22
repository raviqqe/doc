# Bytecode encoding v2 in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

December 22, 2024

---

# Contents

- Stak Scheme
- Progress
  - Bytecode encoding v2
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support the R7RS-small standard.
- Forked from Ribbit Scheme

---

# Progress

- Bytecode encoding v2

---

# Bytecode encoding v1 in Stak Scheme

- In Stak (and Ribbit) Scheme, everything is a list.
- Bytecodes and data values are represented by cons's or scalars (numbers.)
  - Only `if` instructions branch into two preceding instruction lists.
- A program can be considered as a DAG composed of cons's with instruction codes interleaved with data.

We can just encode/decode DAG's of nodes with zero to two edges!

## References

- [A R4RS Compliant REPL in 7 KB, Léonard et al.](https://arxiv.org/pdf/2310.13589)

---

# Bytecode encoding v1

- It is roughly borrowed from Ribbit Scheme.

## Decoding

1. Expand a symbol table.
   - Symbols may or may not have their string representations.
1. Decode instruction lists recursively as lists into memory.
   - When we hit symbols or non-number constants, we look up the symbol table and store their indices into decoded instructions.
1. On initialization, we initialize constants by executing constant initialization logic attached at the beginning of the program.

---

# Bytecode encoding v2

- The new bytecode format is aimed for:
  - Simpler decoding
  - Faster startup time

## Decoding

1. Decode instruction lists recursively as lists into memory.
   - When we hit symbols or non-number constants, we look up the symbol table and store their indices into decoded instructions.

## References

- https://github.com/raviqqe/til/tree/main/dag-encoder

---

# Future work

- ~~Faster startup time~~ Finally!
- Easier integration with Rust
- Better compatibility with the R7RS small

---

# Summary

- Building a bytecode encoder is fun! 😃
