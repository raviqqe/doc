# Memory unsafety in Stak Scheme virtual machines

[@raviqqe](https://github.com/raviqqe)

June 7, 2025

---

# Contents

- Stak Scheme
- Progress
  - Library system in `eval` procedure
  - `case-lambda` syntax
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support the R7RS-small standard.
- Forked from [Ribbit Scheme](https://github.com/udem-dlteam/ribbit)

## References

- [GitHub](https://github.com/raviqqe/stak)
- [Website](https://raviqqe.com/stak)

---

# Progress

- Library system in `eval` procedure
- `case-lambda` syntax

---

---

# Future work

- Synchronous and asynchronous APIs in the same crate
- Unicode support
- Tree shaking
- `define-library` syntax in the command line interpreter
- `include` syntax

---

# Summary

- Building Scheme is fun! 🥳
