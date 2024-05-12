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

# `eval` in R7RS

```scheme
(import (scheme base) (scheme write) (scheme eval))

(eval
  '(display "foo")
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
