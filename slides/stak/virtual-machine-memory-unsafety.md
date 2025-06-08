# Memory unsafety in Stak Scheme virtual machines

[@raviqqe](https://github.com/raviqqe)

June 7, 2025

---

# Contents

- Stak Scheme
- Progress
  - Library system in `eval` procedure
  - `case-lambda` syntax
- Memory unsafety in virtual machines
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

# Library system in `eval` procedure

- Stak Scheme now supports the `define-library` syntax in the `eval` procedure.
- The `eval` procedure creates a new library environment for a given library definition.
- It is implemented by sharing logic of the library system in the bytecode compiler.
  - Compiler inception again :)

---

## Memory unsafety in virtual machines

- Ribbit Scheme's design achieves simplicity, performance, portability, and extensibility at the same time.
  - Stak Scheme adopts the same design and architecture of the language processor.
- What's missing??
  - Security!

---

# Security in Stak Scheme

- The host language of Rust is memory safe.
- So the virtual machine of Stak Scheme is memory safe.
- However, Scheme programs are not memory safe in terms of memory on the virtual machine.
  - Today, we call this VM memory unsafety.
- It is intentionally VM memory unsafe.
  - Performance gets higher without type checks.
  - The unsafety enables primitive operations in bytecodes.
    - e.g. irect manipulation of stacks

---

# Future work

- Unicode support
- `include` syntax
- Tree shaking
- Synchronous and asynchronous APIs in the same crate

---

# Summary

- Building Scheme is fun! 🥳
