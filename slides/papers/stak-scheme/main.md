<!--
theme: default
paginate: true
-->

# Stak Scheme: The tiny R7RS-small implementation

## Scheme Workshop 2025

Yota Toyama

---

# Background

- Ribbit Scheme, the tiny R4RS implementation
  - Bytecode compiler: Scheme
  - Virtual machine: x86-64 assembly, C, Javascript, Bash, etc.
- Can we implement the entire R7RS-small standard on its VM? 🤔
  - Yes, we can!

---

# Stak Scheme

- Stak Scheme, the tiny R7RS-small implementation

## Comparison with Ribbit Scheme

|                   | Stak   | Ribbit         |
| ----------------- | ------ | -------------- |
| Data structure    | Pair   | Rib            |
| Bytecode encoding | Pair   | Rib            |
| Compiler          | Scheme | Scheme         |
| VM                | Rust   | Many languages |

---

# Virtual machine

- A stack machine
- **Everything is a pair**.
  - Bytecode
  - Values
    - e.g. lists, characters, strings, etc.
  - A stack
- Binary-level homoiconicity
- "Von Neumann architecture"

---

# Code graph

- A representation of a Scheme program on memory
- Directed Acyclic Graph (DAG)
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

# Compactness

|       | Lines of code (KLOC) | Binary size (KB) |
| ----- | -------------------: | ---------------: |
| mstak |                9,127 |          108,648 |
| tr7i  |               16,891 |          301,536 |

## References

- [TR7: tiny R7RS-small scheme interpreter](https://jobol.gitlab.io/tr7/)

---

# Appendix

---

# Fibonacci function

![](./fibonacci.svg)

---

# References

- https://en.wikipedia.org/wiki/Homoiconicity
