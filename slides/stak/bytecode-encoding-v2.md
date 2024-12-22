# Bytecode encoding v2 in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

December 24, 2024

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

# Bytecode encoding in Stak Scheme

- In Stak (and Ribbit) Scheme, everything is a list.
- Bytecodes and data values are represented by cons's or scalars (numbers.)
- A program can be considered as a DAG composed of cons's with instruction codes interleaved with data.

We can just encode/decode DAG's of nodes with zero to two edges!

---

# Bytecode encoding in Ribbit Scheme

## Decoding

1. Expand a symbol table.
   - Symbols may or may not have their string representations.
1. foo

---

# Bytecode encoding v2

- The new bytecode format is aimed for:
  - Simpler decoding
  - Faster startup time

## Decoding

1. Encode a node

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
