# Inception: The self-embedding compiler in Stak Scheme

![A spinning top](https://raviqqe.s3.amazonaws.com/doc/stak/inception/spinning_top_3.jpg)

Code duplication is one of the primary sources for bugs and maintenance burden. If a language processor needs to embed itself into its target codes, what can we do?

[Stak Scheme][stak] is a Scheme interpreter written in Rust aiming for small memory footprints which I've been developing in the last few years.
While I implemented it originally as a re-implementation of [Ribbit Scheme][ribbit], I was wondering how to avoid code duplication. This article depicts how [Stak Scheme][stak]'s bytecode compiler embeds itself into the `eval` procedure in the target codes.

## TL;DR

- Handling a compiler itself as data in the compiler allows a compiler itself to be embedded into target codes.
- S-expressions and its homoiconicity is great. Lisp all the things! 😃

## Code duplication of compilers between the compiler command and the `(scheme eval)` library

The reason why we have this architecture of a bytecode compiler in Scheme and a virtual machine in Rust is to strip unnecessary libraries at the bytecode level. In R7RS, you can define libraries using the `define-library` syntax.

> WIP

## Embedding a compiler into its target codes

The basic idea to resolve the code duplication problem between the bytecode compiler and the `eval` library is simply to copy the compiler into source codes it compiles.

The basic structure of the new `eval` procedure looks like the following.

```scheme
(define eval
  (let ((compile ($$compiler)))
    (lambda (expression environment)
      ((compile expression environment)))))
```

You see the `($$compiler)` primitive call in the middle of the function body. The bytecode compiler replaces the primitive call with source codes of the compiler in S-expressions right after reading source codes. The reason it is not just a `$$compiler` symbol but wrapped with parentheses making it a form of a procedure call is that we cannot differentiate such a symbol from normal uses of the symbol; the compiler itself contains the `$$compiler` symbol to replace it in source codes!

## The other solutions?

Although this is specific to the architecture of Stak Scheme, it is technically possible to compile source codes of the compiler and target codes separately and concatenate them together into a single chunk of target codes. However, this approach has a several downsides.

First, the bytecode compiler of Stak Scheme (or Ribbit Scheme) does not have any easy way to concatenate bytecodes. Because the bytecode compiler compiles source codes into bytecodes in its in-memory format of Directed Acyclic Graph (DAG) and then encodes them into its serialized format, bytecode concatenation is not as simple as concatenation of byte arrays.

Secondly, WIP

## References

- [Stak Scheme][stak]
- [Ribbit Scheme][ribbit]

[stak]: https://github.com/raviqqe/stak
[ribbit]: https://github.com/udem-dlteam/ribbit
