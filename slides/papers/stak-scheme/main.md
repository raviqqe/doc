<!--
theme: default
class: invert
paginate: true
-->

# Stak Scheme: The tiny R7RS-small implementation

## Scheme Workshop 2025

Yota Toyama

---

# Background

- Ribbit Scheme, the tiny R4RS implementation
  - Bytecode compiler: Scheme
  - Virtual Machine (VM): x86-64 assembly, C, Javascript, Bash, etc.
- Can we implement the entire R7RS-small standard on the Ribbit VM? 🤔
  - Yes, we can!

---

# Stak Scheme

- Stak Scheme, the tiny R7RS-small implementation

## Comparison to Ribbit Scheme

|                   | Stak          | Ribbit                                   |
| ----------------- | ------------- | ---------------------------------------- |
| Data structure    | Pair          | Rib                                      |
| Bytecode encoding | Local caching | Global caching + constant initialization |
| Compiler          | Scheme        | Scheme                                   |
| VM                | Rust          | Many languages                           |

---

# Virtual machine

- A stack machine
- **Everything is a pair**.
  - Bytecode
  - Values
    - Lists, characters, strings, etc.
  - A stack
- Binary-level homoiconicity
- "Von Neumann architecture"

---

# Code graph

- A representation of a Scheme program on memory
- Directed Acyclic Graph (DAG) of pairs
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

![h:450px](./if-instruction.svg)

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

![h:450px](./duplicate-strings.svg)

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

# Library system

## Code graph

![h:450px](./library-system.svg)

---

# Code graph in depth

- A pair consists of `car`, `cdr`, and a tag on the side of `cdr`.
  - Tags represent either instructions or data types.
- Universal representation for both in-memory bytecode and Scheme values

![](code-graph-in-depth.svg)

<!--
# Examples

> WIP
-->

---

# Encoding & decoding

- The compiler encodes a code graph into a byte sequence.
- The VM decodes a code graph into a byte sequence.

![h:180px](encode.svg)

![h:180px](decode.svg)

---

# Encoding merges

![](merge.svg)

---

# `eval` and compiler

- The compiler from S-expression to code graph is **data**.
- `(incept source)` embeds the compiler as a library into source code.
- `((eval compiler) source)` compiles the source code.

![bg right:30% h:650px](eval.svg)

---

# Compactness

|       | Lines of code (KLOC) | Binary size (KB) |
| ----- | -------------------: | ---------------: |
| mstak |                9,127 |          108,648 |
| tr7i  |               16,891 |          301,536 |

## References

- [TR7: tiny R7RS-small scheme interpreter](https://jobol.gitlab.io/tr7/)

---

# Future work

> WIP

---

# Appendix

---

# Fibonacci function

![](./fibonacci.svg)

---

# References

- https://en.wikipedia.org/wiki/Homoiconicity
