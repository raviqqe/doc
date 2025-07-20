# Tree shaking in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

July 20, 2025

---

# Contents

- Stak Scheme
- Progress
- Tree shaking
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
- The compiler is written in Scheme.
- The VM is written in Rust.
- It aims to support R7RS-small.

---

# Progress

- Tree shaking
- Unicode (UTF-8) support in I/O
- The `include` syntax

---

# Tree shaking

- Scheme is an **impure** functional programming language.
- Any procedure calls might have side effects in Scheme.
- Even in libraries, we might have top-level procedure calls.

---

# Future work

- Debug mode
  - Stack trace
- Unicode in the `(scheme char)` library
