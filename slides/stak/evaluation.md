# `eval` in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

February 18, 2024

---

# Contents

- Stak Scheme
- Library system in R7RS
- Implementation
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support R7RS-small.

---

# Progress

- The `eval` procedure
  - Only procedures available
  - No macro support yet
- The `stak-profile` command
  - Traces and profiles Stak Scheme codes.

---

# `eval` in R7RS

- The `eval` procedure evaluates an S-expression.
- Only global bindings in a given environment are accessible.

```scheme
(eval <expr-or-def> <environment>)
```

```scheme
(import (scheme base) (scheme write) (scheme eval))

(eval
  '(display "Hello, world!")
  (environment '(scheme write)))
```

---

# Options

# #1

- Disassemble procedures in a compiler into target codes.
- No portability of the compiler
  - The other Scheme implementation cannot be used to run the compiler.

# #2

# #3

---

# Demo

---

# Future work

- Macros in `eval`
- Duduplication of codes between a compiler and the `(scheme eval)` library

---

# Summary

- Building `eval` is fun!
