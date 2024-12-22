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

# Bytecode encoding in Ribbit Scheme

## Decoding

1. Expand a symbol table.
   - Symbols may or may not have their string represenations.
1. foo

---

# Bytecode encoding v2

- The new bytecode format is aimed for:
  - Faster startup time

---

# Future work

- ~~Faster startup time~~ Finally!

---

# Summary

- Building an interpreter!
