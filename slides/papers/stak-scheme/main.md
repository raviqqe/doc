<!--
theme: default
class: invert
paginate: true
-->

<style>
strong {
  color: lightpink;
}
</style>

# Stak Scheme: The tiny R7RS-small implementation

## Scheme Workshop 2025

Yota Toyama

---

# Background

- [Ribbit Scheme, the tiny R4RS implementation][ribbit]
  - Simple, portable, compact, and fast
- Two components
  - Compiler: Scheme
  - Virtual Machine (VM): x86-64 assembly, C, Javascript, Bash, ...

---

# Ribbit Scheme in depth

- Ribbit Virtual Machine (RVM)
  - A stack machine
- **Everything is a list**.
  - Code
  - Values
    - Lists, characters, strings, ...
  - Call/value stacks
- "Von Neumann architecture"
  - Both code and data in heap

---

# Code graph

- A representation of a Scheme program on memory
- Universal between code and data
  - e.g. no special garbage collection for code
- Directed Acyclic Graph (DAG) of pairs
- Used at both **compile time** in the compiler and **runtime** in the VM.

![](./fibonacci.svg)

---

# Can we implement the entire R7RS-small standard on the Ribbit VM? 🤔

---

# Yes, we can! 😏

---

# Stak Scheme

- Stak Scheme, the tiny R7RS-small implementation
  - Simple, portable, compact, and fast
- Open source on GitHub: [`raviqqe/stak`][stak]

## Comparison to Ribbit Scheme

|                      | Stak           | Ribbit                               |
| -------------------- | -------------- | ------------------------------------ |
| Compiler             | Scheme         | Scheme                               |
| VM                   | Rust           | Many languages                       |
| Data structure       | Pair (doublet) | Rib (triplet)                        |
| **Program encoding** | Dynamic cache  | Global cache + continuation/constant |

---

# Compiling and running a program

- A compiler compiles source code into a serialized code graph.
- The VM deserializes it into a **code graph**.

![h:180px](compile.svg)

![h:180px](run.svg)

---

# Code graph

- A representation of a Scheme program on memory
  - Universal between code and data
    - e.g. no special garbage collection for code
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

# Encoding & decoding

- A code graph is encoded by a topological sort.
- The compiler encodes a code graph into bytecode.
- The VM decodes bytecode into a code graph.

![h:180px](encode.svg)

![h:180px](decode.svg)

---

# Encoding shared nodes

- Shared nodes are cached _locally_ and _dynamically_.
- On the first visit, the pair is added to cache.
- On the last visit, the pair is removed from cache.

![](merge.svg)

---

# `eval` and compiler

- The compiler from S-expression to code graph is **data**.
- `(incept source)` embeds the compiler as a library into source code.
- `((eval compiler) source)` compiles the source code.

![bg right:30% h:650px](eval.svg)

---

# Compactness

|       | Lines of code (LOC) | Binary size (KB) |
| ----- | ------------------: | ---------------: |
| mstak |               9,127 |          108,648 |
| tr7i  |              16,891 |          301,536 |

## References

- [TR7: tiny R7RS-small scheme interpreter](https://jobol.gitlab.io/tr7/)

---

# Future work

> WIP

---

# Acknowledgements

Huge thanks 🙏 to:

- The authors of Ribbit Scheme
  - Especially, the dynamic programming language team at the University of Montréal
- Léonard Oest O’Leary and William E. Byrd for early comments on the draft
- [@sisshiki1969](https://github.com/sisshiki1969) and [@yhara](https://github.com/yhara) for discussions on the language processor design

---

# References

- [Ribbit Scheme][ribbit]

[ribbit]: https://github.com/udem-dlteam/ribbit
[stak]: https://github.com/raviqqe/stak
[homoiconicity]: https://en.wikipedia.org/wiki/Homoiconicity

---

# Appendix

---

# Virtual machine

- A stack machine
- Everything is a **pair**.
  - Code graph
  - Values
    - Lists, characters, strings, ...
  - A stack
- Binary-level [homoiconicity][homoiconicity]
- "Von Neumann architecture"

---

# Code graph in depth

- A pair consists of `car`, `cdr`, and a tag on the side of `cdr`.
  - Tags represent either instructions or data types.
- Universal representation for both in-memory bytecode and Scheme values

![](code-graph-in-depth.svg)

---

# Fibonacci function

![](./fibonacci.svg)
