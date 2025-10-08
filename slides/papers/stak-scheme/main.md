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

<!--
In this talk, I would like to introduce a new R7RS Scheme implementation called Stak Scheme.

It is primarily designed as an embedded scripting language for Rust.

But it can also run by itself as a command line interpreter.
-->

---

# Background

- [Ribbit Scheme, the tiny R4RS implementation][ribbit]
  - Simple, portable, compact, and fast
- Two components
  - Compiler: Scheme
  - Virtual Machine (VM): x86-64 assembly, C, Javascript, Bash, ...

<!--
As a background, a few years ago, the research team at University of Montréal published Ribbit Scheme, the tiny R4RS implementation.

It aims to be simple, portable, compact, and fast.

In Ribbit Scheme, one of the primary features is the split architecture of the compiler and the virtual machine.

The compiler compiles source code into something like bytecode.

The virtual machine runs the bytecode as a Scheme program.
-->

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

<!--

-->

---

# Code graph

- A representation of a Scheme program on memory
  - Directed Acyclic Graph (DAG) of **Ribs** (i.e. pairs)
- Universal between **code** and **data**
  - e.g. no special garbage collection for code

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

|                         | Stak           | Ribbit                               |
| ----------------------- | -------------- | ------------------------------------ |
| Data structure          | Pair (doublet) | Rib (triplet)                        |
| **Code graph encoding** | Local cache    | Global cache + continuation/constant |

---

# Compiling & running a program

- A compiler compiles source code into an encoded **code graph**.
- The VM decodes and runs it as a program.
- Code graphs are used at both in the compiler and on the VM.

![h:160px](compile.svg)

![h:160px](run.svg)

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
- The compiler encodes a code graph into bytes.
- The VM decodes bytes into a code graph.

![h:160px](encode.svg)

![h:160px](decode.svg)

---

# Encoding shared nodes

- Shared nodes are cached _locally_.
- On the first visit, a node is added to cache.
- On the last visit, the node is removed from cache.

![](merge.svg)

---

# `eval` & the compiler

- The compiler from S-expression to code graph is **data**.
- `(incept compiler source)` **embeds the compiler** into source code.
- `((eval compiler) source)` compiles the source code into a code graph.

![](eval.svg)

---

# Don't forget macros...

- `define-syntax` defines user-defined macros.
- Macros and libraries can be expanded statically.
- `eval` needs to know their definitions.
  - Transfer macros and libraries from the compiler to the VM.

---

# Compactness

|       | Lines of code (LOC) | Binary size (KB) |
| ----- | ------------------: | ---------------: |
| mstak |               9,127 |          108,648 |
| tr7i  |              16,891 |          301,536 |

## References

- [TR7: tiny R7RS-small scheme interpreter](https://jobol.gitlab.io/tr7/)

---

# Benchmarks

| Benchmark | mstak | stak | mstak (embed) | stak (embed) | tr7i |  gsi | chibi | gosh |
| --------- | ----: | ---: | ------------: | -----------: | ---: | ---: | ----: | ---: |
| empty     |  1.00 | 1.04 |          0.14 |         0.38 | 0.77 | 0.51 |  3.63 | 1.27 |
| hello     |  1.00 | 1.04 |          0.13 |         0.36 | 0.73 | 0.53 |  9.84 | 3.62 |
| fibonacci |  1.00 | 1.12 |          0.96 |         1.05 | 1.35 | 1.66 |  0.93 | 0.45 |
| sum       |  1.00 | 1.13 |          1.01 |         1.06 | 1.19 | 1.64 |  0.98 | 0.24 |
| tak       |  1.00 | 1.09 |          0.89 |         0.98 | 0.96 | 1.23 |  1.21 | 0.54 |

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
