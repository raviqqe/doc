# Stak Scheme: The tiny R7RS-small implementation

## Scheme Workshop 2025

Yota Toyama

---

# Stak Scheme

---

# Background

- Ribbit Scheme is a tiny R4RS implementation.
  - The bytecode compiler is written in Scheme.
  - The virtual machine is written in some host language.
- Can we implement the entire R7RS-small standard on its VM? 🤔

---

# Stak Scheme

- Stak Scheme is the tiny R7RS-small implementation.
  - The bytecode compiler is written in Scheme.
  - The virtual machine is written in Rust.

---

# Virtual machine

- A stack machine
- **Everything is a pair**.
  - Bytecode
  - Values
    - Including lists, characters, strings, etc.
- VM-level homoiconicity

---

# Code graph

> WIP

---

# Examples

---

# If instruction

## Scheme

```scheme
(display (if x "foo" "bar"))
```

---

# If instruction

## Code graph

![](./if-instruction.svg)

---

# Duplicate strings

## Scheme

```scheme
(display "foo")
(display "foo")
(display "bar")
```

---

# Duplicate strings

## Code graph

![](./duplicate-strings.svg)

---

# Library system

## Scheme

```scheme
(define-library (foo)
  (export foo)

  (begin (define foo 42)))

(define-library (bar)
  (export foo)

  (begin (define foo 42)))

(import (foo) (prefix (bar) bar-))

(+ foo bar-foo)
```

---

## Library system

### Code graph

![](./library-system.svg)

---

# Appendix

---

# Fibonacci function

![](./fibonacci.svg)

---

# References

- https://en.wikipedia.org/wiki/Homoiconicity
