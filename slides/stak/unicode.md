# Unicode in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

November 15, 2025

---

# Contents

- Stak Scheme
- `eval` in R7RS
- Implementation in Stak Scheme
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support R7RS-small.

---

# Progress

- Backtrace on errors
- Unicode support

---

# Backtrace

```scheme
(import (scheme base))

(define (foo)
  (error "Oh, no!" 42)
  #f)

(define (bar)
  (foo)
  #f)

(bar)

; ...

```
