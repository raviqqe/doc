# Inception: The self-embedding compiler in Stak Scheme

Code duplication is one of the primary sources for bugs and maintenance burden. If a language processor needs to embed itself into its target codes, what can we do?

[Stak Scheme][stak] is a Scheme interpreter written in Rust aiming for small memory footprints which I've been developing in the last few years.
While I implemented it orignally as a re-implementation of [Ribbit Scheme][ribbit], I was wondering how to avoid code duplication

## TL;DR

- Handling a compiler itself as data in the compiler allows you.
- S-expressions and its homoiconicity is great. 😃

## The problem

## Inception

## The other solutions

## Refereces

- [Stak Scheme][stak]
- [Ribbit Scheme][ribbit]

[stak]: https://github.com/raviqqe/stak
[ribbit]: https://github.com/udem-dlteam/ribbit
