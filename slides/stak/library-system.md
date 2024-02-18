# Embedding Stak Scheme in Rust

[@raviqqe](https://github.com/raviqqe)

January 14, 2024

---

# Contents

- Library system
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
- The compiler is written in Scheme.
- The VM is written in Rust.
- It aims to support R7RS-small.

---

# Library system

- A library system

```scheme
(define-library (foo)
  (export foo)
  (import (scheme base))

  (begin
    (define (foo x)
      (write-u8 x))))

(import (foo))

(foo 65)
```

---

# Future work

- `eval` procedure

---

# Summary

- Buildling a library system is fun!
