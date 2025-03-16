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
- Examples
- Future work

---

# Examples

- [Detect features #2154](https://github.com/raviqqe/stak/pull/2154)
- [Make non-data symbols empty #2168](https://github.com/raviqqe/stak/pull/2168)

---

# Future work

- [Segregate environment for each `eval` call](https://github.com/raviqqe/stak/issues/1997)
- [The "Inception" project](https://github.com/raviqqe/stak/issues/2157)

---

# Summary

- Building Rust integration is fun! 😃
