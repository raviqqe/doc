# Embedding Stak Scheme in Rust

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

# Library system in R7RS

## Defining a library

- Libraries export symbols.
- Libraries import symbols from other libraries.
- The main script import libraries.
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

## Importing a library

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

# Implementation in a compiler

## Pipelines

1. Read source.
1. Expand libraries. <- new!
   - Read all `(define-library ...)` clauses.
   - Expand all `(import ...)` clauses.
1. Expand macros.
1. Compile expressions.
1. Encode objects.
1. Write bytecodes.

---

# Future work

- Library system
  - `(rename ...)`
  - `(prefix ...)`
- `eval` procedure

---

# Summary

- Buildling a library system is fun!
