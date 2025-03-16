# New bytecode decoder for Stak Scheme

[@raviqqe](https://github.com/raviqqe)

March 15, 2025

---

# Contents

- Stak Scheme
- Progress
  - `any-fn` crate
  - Rust integration
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support the R7RS-small standard.
- **Aims to be an interpreter with small memory footprint**

---

# Progress

- New bytecode decoder
- Rust integration in Stak Scheme

---

# Examples

- [Detect features #2154](https://github.com/raviqqe/stak/pull/2154)
- [Make non-data symbols empty #2168](https://github.com/raviqqe/stak/pull/2168)

---

# Future work

- Auto conversion of primitive types
  - e.g. usize, f64
- Garbage collection of foreign objects

---

# Summary

- Building Rust integration is fun! 😃
