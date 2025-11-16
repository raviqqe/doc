<!--
class: invert
-->

# Unicode in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

November 15, 2025

---

# Contents

- Stak Scheme
- Progress
  - Backtrace on errors
  - Unicode in `(scheme char)`
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
- Unicode in `(scheme char)`

---

# Backtrace on errors

```scheme
(import (scheme base))

(define (foo)
  (error "Oh, no!" 42)
  #f)

(define (bar)
  (foo)
  #f)

(bar)
```

```
Oh, no! 42 [error foo eval #f]
Error: halt
```

---

# Future work

> WIP
