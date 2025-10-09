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
In this talk, I would like to introduce a new tiny R7RS Scheme implementation called Stak Scheme.
-->

---

# Background

- [Ribbit Scheme, the tiny R4RS implementation][ribbit]
  - Simple, portable, compact, and fast
  - R4RS REPL in 7 KB
- Two separate components
  - Compiler written in Scheme
  - Virtual Machine (Ribbit VM, or just **RVM**) written in x86-64 assembly, C, Javascript, Bash, ...

<!--
A few years ago, the research team at University of Montréal published Ribbit Scheme, the tiny R4RS implementation.

Its VM aims to be simple, portable, compact, and fast at the same time.

In Ribbit Scheme, one of the primary features is the split architecture of the compiler and the virtual machine.

The compiler compiles source code in Scheme into bytecode.

The virtual machine runs the bytecode as a Scheme program.
-->

---

# Can we implement the entire R7RS-small standard on RVM? 🤔

<!--
RVM is very compact and reasonably fast.

The question is, can we implement the entire R7RS-small, the latest standard of Scheme, on RVM?

-->

---

# Yes, we can (or did)! 😏

<!--
And the answer is yes, we did.
-->

---

# Stak Scheme

- Stak Scheme, the tiny **R7RS-small** implementation
  - Simple, portable, compact, and fast
- Two use cases
  - Embedded scripting language
  - Standalone interpreter
- Open source on GitHub: [`raviqqe/stak`][stak]

|                   | Stak              | Ribbit                               |
| ----------------- | ----------------- | ------------------------------------ |
| Bytecode encoding | Local cache       | Global cache + continuation/constant |
| `eval` procedure  | Uses the compiler | Separate from the compiler           |

<!--
So that's why we developed Stak Scheme, the tiny R7RS-small implementation.

Its purpose is basically the same as Ribbit Scheme.

It aims to be simple, portable, compact, and fast.

But it also implements the entire R7RS-small standard.

Stak Scheme is primarily designed as an embedded scripting language.

But it can also run by itself as a command line interpreter.
-->

---

# Ribbit VM in depth

- Ribbit VM (RVM)
  - A stack machine
- **Everything is a list**.
  - Code
  - Values
    - Lists, characters, strings, ...
  - Call/value stacks
- "Von Neumann architecture"
  - Both code and data in heap

<!--
Before going into the biggest differences between Stak Scheme and Ribbit Scheme, let me explain a bit more about Ribbit VM.

Ribbit Scheme's virtual machine is called Ribbit Virtual Machine, which is a typical stak machine.

And we use the same one for Stak Scheme.

Something interesting about RVM is that everything is a list including all Scheme values, Scheme program, and stacks to define the state of the virtual machine.

In other words, RVM adopts "Von Neumann architecture" in a way.

In the heap memory, everything is structured as lists while we have flat memory blocks on linear memory.

You can manipulate code and data on RVM in exactly the same way.
-->

---

# Code graph

- A representation of a Scheme program on memory
  - Directed Acyclic Graph (DAG) of **pairs**
- Contains both **code** and **data**.
  - e.g. no special interpreter for `eval`

![](./fibonacci.svg)

<!--
On RVM, a Scheme program is represented as a data structure called a code graph, which is basically a DAG of pairs.

This example is the one of the fibonacci function implemented on RVM.

And, as I mentioned before, we use this code graph for both code and data.

Because of that, for example, when we want to implement the `eval` procedure,
we simply compile an S-expression into a code graph, and execute it as a procedure.

I'm gonna talk more about it later.
-->

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

```scheme
(display (if x "foo" "bar"))
```

![h:450px](./if-instruction.svg)

---

# Duplicate strings

```scheme
(display "foo")
(display "foo")
(display "bar")
```

![h:350px](./duplicate-strings.svg)

---

# Library system

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

# R4RS vs R7RS-small

- R4RS lacks some good programming constructs...
- R7RS-small adds:
  - Hygienic macros
    - i.e. `define-syntax` and `syntax-rules`
  - Library system
  - More built-in procedures and libraries

<!--
So Ribbit Scheme implements R4RS, which is a bit old standard of Scheme.

And Stak Scheme implements the latest R7RS-small standard.

When it comes to the differences between R4RS and R7RS-small,
R7RS-small added some big functionalities like hygienic macros, and the library system.
-->

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
- And, of course, all the reviewers!

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
