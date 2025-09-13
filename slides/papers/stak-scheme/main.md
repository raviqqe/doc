# Stak Scheme: The tiny R7RS-small implementation

## Scheme Workshop 2025

Yota Toyama

<!--
# Table of contents

- Background
- Stak Scheme
- Code graph
-->

---

# Background

- Ribbit Scheme is a tiny R4RS implementation.
  - The bytecode compiler is written in Scheme.
  - The virtual machine is written in some host language.
- Can we implement the entire R7RS-small standard on its VM? 🤔
  - => Yes, we can!

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
    - e.g. lists, characters, strings, etc.
  - A stack
- VM-level homoiconicity

---

# Code graph

- A representation of a Scheme program on memory
- Used at both **compile time** in the compiler and **runtime** in the VM.

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

![height:450px](./if-instruction.svg)

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

![height:450px](./duplicate-strings.svg)

---

# Library system

## Scheme

```scheme
(define-library (foo)
  (export foo)

  (begin
    (define foo 123)))
```

```scheme
(import (prefix (foo) bar-))

(define foo 456)

(+ bar-foo foo)
```

---

## Library system

### Code graph

![height:450px](./library-system.svg)

---

# Code graph in depth

- A pair consists of `car`, `cdr`, and a tag on the side of `cdr`.
- Again, this representation is used for everything including in-memory bytecode and Scheme values.

![](code-graph-in-depth.svg)

---

# Encoding & decoding

- The compiler encodes a code graph into a byte sequence.
- The VM decodes a code graph into a byte sequence.

![](encode-decode.svg)

---

# Appendix

---

# Fibonacci function

![](./fibonacci.svg)

---

# References

- https://en.wikipedia.org/wiki/Homoiconicity
