# Inception: The self-embedding compiler in Stak Scheme

![A spinnning top](https://raviqqe.s3.amazonaws.com/doc/stak/inception/spinning_top_1.jpg)

Code duplication is one of the primary sources for bugs and maintenance burden. If a language processor needs to embed itself into its target codes, what can we do?

[Stak Scheme][stak] is a Scheme interpreter written in Rust aiming for small memory footprints which I've been developing in the last few years.
While I implemented it originally as a re-implementation of [Ribbit Scheme][ribbit], I was wondering how to avoid code duplication. This article depicts how [Stak Scheme][stak]'s bytecode compiler embeds itself into the `eval` procedure in the target codes.

## TL;DR

- Handling a compiler itself as data in the compiler allows a compiler itself to be embedded into target codes.
- S-expressions and its homoiconicity is great. Lisp all the things! 😃

## The problem

> WIP

## Inception

> WIP

## The other solutions

Although this is specific to the artchitecture of Stak Scheme, it is technically possible to compile the compiler in S expressions and target codes separately and concatenate them together into a single chunk of target codes. However, this approach has a several downsides.

First, the bytecode compiler of Stak Scheme (or Ribbit Scheme) does not have any easy way to concatenate bytecodes. Because the bytecode compiler compiles source codes into bytecodes in its in-memory format of Directed Acyclic Graph (DAG) and then encodes them into its serialized format, bytecode concatenation is not as simple as concatenation of byte arrays.

Secondly, WIP

## References

- [Stak Scheme][stak]
- [Ribbit Scheme][ribbit]

[stak]: https://github.com/raviqqe/stak
[ribbit]: https://github.com/udem-dlteam/ribbit
