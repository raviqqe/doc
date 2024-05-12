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

## Defining a library

- Libraries export symbols.
- Libraries import symbols from other libraries.
- Libraries are "called" but only once.

```scheme
(define-library (foo)
  (export foo)

  (import (scheme base))

  (begin
    (define (foo x)
      (write-u8 x))))
```

---

# Library system in R7RS

## Importing a library at a top level

```scheme
(import (foo))

(foo 65)
```

---

# Where to put libraries?

- Where to put libraries?
  - Inlining library clauses (e.g. Gauche)
  - Libraries as files (e.g. Chibi Scheme)
- Stak Scheme took the inlining solution.

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

# Options

# #1

- Disassemble a compiler into
- No portability of the compiler
  - The other Scheme implementation cannot be used to run the compiler.

# #2

# #3

---

# Future work

- Macros in `eval`
- Duduplication of codes between a compiler and the `(scheme eval)` library

---

# Summary

- Building `eval` is fun!
