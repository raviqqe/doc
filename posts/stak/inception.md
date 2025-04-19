# Inception: The self-embedding compiler in Stak Scheme

![A spinning top](https://raviqqe.s3.amazonaws.com/doc/stak/inception/spinning_top_3.jpg)

Code duplication is one of the primary sources for bugs and maintenance burden in software engineering. If a language processor itself is its target codes, what can we do?

In the last few years, I've been developing a Scheme interpreter in Rust called [Stak Scheme][stak].
The Scheme interpreter aims for small memory footprints with [reasonable performance](https://github.com/raviqqe/stak?tab=readme-ov-file#performance) and to be a complete implementation of the [R7RS-small][r7rs-small] standard.
While I implemented it originally as a re-implementation of [Ribbit Scheme][ribbit], I was wondering how to avoid code duplication of its compiler and the `eval` library. This article depicts how Stak Scheme's bytecode compiler "incepts" itself into the `eval` procedure in its target codes.

## TL;DR

- Handling a compiler itself as data in the compiler itself allows it to be embedded into target codes.
- S-expressions and its homoiconicity is stunning. Lisp all the things :)

## Code duplication between the compiler command and the `eval` library

First things first, Ribbit and Stak Scheme has an irregular architecture of the whole language processor; it is composed of a bytecode compiler written in Scheme and a virtual machine (VM) in Rust. The benefit of this architecture is that we can omit a big chunk of logic from resulting artifacts, such as executable binaries. Because library expansion, macro expansion, optimization, and actual compilation down to bytecodes are all inside the compiler split from a VM, we do not have to put the extra bytes for the logic into the artifacts. I believe this is a key decision made by the research team of Ribbit Scheme for the goal of small memory footprints.

The reason why we have this architecture of a bytecode compiler in Scheme and a virtual machine in Rust is to strip unnecessary libraries at the bytecode level. In R7RS, you can define libraries using the `define-library` syntax.

> WIP

## "Incepting" a compiler into its target codes

The basic idea to resolve the code duplication problem between the bytecode compiler and the `eval` library is simply to copy the compiler into source codes it compiles.

The basic structure of the new `eval` procedure looks like the following.

```scheme
(define eval
  (let ((compile ($$compiler)))
    (lambda (expression environment)
      ((compile expression environment)))))
```

You see the `($$compiler)` primitive call in the middle of the function body. The bytecode compiler replaces the primitive call with source codes of the compiler in S-expressions right after reading source codes. The reason it is not just a `$$compiler` symbol but wrapped with parentheses making it a form of a procedure call is that we cannot differentiate such a symbol from normal uses of the symbol; the compiler itself contains the `$$compiler` symbol to replace it in source codes!

## The other solutions

### Concatenating bytecodes

It is technically possible to compile source codes of the compiler and target codes separately and concatenate them together into a single chunk of target codes. However, this approach has a several downsides.

First, the bytecode compiler of Stak Scheme (or Ribbit Scheme) does not have any easy way to concatenate bytecodes as byte sequences. Because the bytecode compiler compiles source codes into bytecodes in its in-memory format of Directed Acyclic Graph (DAG) and then encodes them into its serialized format, bytecode concatenation is not as simple as concatenation of byte arrays. In Stak Scheme, bytecodes are not simple sequences of instructions but they represent codes and data in a more natural way. For example, conditional jumps are nodes connected with two outgoing edges leading to nodes of next instructions for its true and false cases.

Secondly, if the bytecode format changes we would need to change the way to seam the two chunks of bytecodes together, which is another maintenance burden.

### Compiler as a library

Another option is providing a compiler as a library specifically inside one of standard libraries, such as `(scheme eval)`, and a compiler simply calls the "compile" procedure in the library. However, this approach is problematic in the case we update the bytecode format because Stak Scheme is self-hosted. If the host side of Stak Scheme uses the old version of bytecodes but we change the new bytecode format of VM in Rust, we cannot do anything. Also, it makes the compiler dependent on Stak Scheme itself and we will not be able to use the other implementations of R7RS Scheme as a compiler host anymore. When a compiler is embedding itself, we don't need to care about such incompatibility between the compiler, VM, and the host implementation of Scheme.

## Next steps

> WIP

## References

- [Stak Scheme][stak]
- [Ribbit Scheme - A portable, compact and extensible Scheme implementation that is fully R4RS compliant.][ribbit]

[stak]: https://github.com/raviqqe/stak
[ribbit]: https://github.com/udem-dlteam/ribbit
[r7rs-small]: https://small.r7rs.org/
