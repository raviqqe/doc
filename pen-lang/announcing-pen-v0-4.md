# Announcing the Pen programming language v0.4

I'm excited to announce [a v0.4 release of the Pen programming language](https://github.com/pen-lang/pen/releases/tag/v0.4.0) here! I've been working on [a new programming language called Pen][pen] for almost a year. And recently, we've released its v0.4 with new syntax constructs, standard packages, and complementary toolls like formatter and documentation generator.

In this post, I would like to introduce the Pen programming language to new people, and describe the current status and new features included in the release.

## Introduction

The Pen programming language is a programming language for _scalable_ software development. It's a statically typed functional programming language with automatic precise memory management. It aims to make development of large-scale applications easy and scalable by focusing on **maintainability** and **portability** of software. Programs written in Pen should be simple, testable, and flexible against changes.

The language that influenced Pen the most is [Go](https://go.dev/). You can also call Pen as a **functional** descendant of the Go programming language with focus on **application programming**. Pen shares the same goal of Go like simplicity, reliability, efficiency, etc. Though, Pen explicitly excludes system programming from its domain to pursue further simplicity.

## Current status

Pen is still under heavy development. Currently, the language is at the stage of finalizing its syntax. Although the language syntax started with a very small number of constructs at the beginning. We've been adding more and more operations

We have quite a few standard libraries too now.

## Changes in v0.4

Since a v0.4 release of the language had been blocked by the [LLVM 14](https://releases.llvm.org/14.0.0/docs/ReleaseNotes.html) release, it includes many new features in the language and complementary tools. Here are some major features of them.

### The Perceus reference counting GC

One of the biggest changes in a compiler is adoption of [the Perceus reference counting GC][perceus]. It is one of the state-of-the-art GC algorithms for functional programming languages.

> WIP

### `go` expression

> WIP

### Formatter

Now, Pen is equipped with its official formatter command of `pen format`. This command formats each source file or ones in a directory of the entire package at once. It can also receive source from standard inputs so that it integrates with existing editors and IDEs easily.

Similarly to Go's `gofmt` and differently from some other formatters like `rustfmt` in Rust, the source formatter of Pen does not define any canonical form of source codes given the same tokens. But it rather tries to align indents, and add and remove spaces and newlines given hints extracted from the original source codes so that developers can still control their source codes to make them look beautiful in their own contexts.

### Documentation generator

> WIP

### `Http` and `Sql` standard packages

In addition to new functions and types in the `Core` and `Os` standard packages, we've added `Http` and `Sql` standard packages. As their names suggest, the `Http` package provides HTTP server and client and the `Sql` package provides a `Sql` client.

Because those packages are dependent on the third-party crates ([`hyper`](https://github.com/hyperium/hyper) and [`sqlx`](https://github.com/launchbadge/sqlx) respectively) in Rust, they are planned **not** to be included the default installation bundle of the language. But they are likely to be separated into different Git repositories.

### LLVM upgrade to 14

As I mentioned above, LLVM was upgraded to 14 in the new release, which fixed many bugs including the ones related to [tail call optimization](https://github.com/raviqqe/llvm-tail-call-opt-bug). Examples of fixed bugs are:

- Certain operations on the built-in map type led to segmentation faults.
- `pen test` command failed on macOS with multiple tests.
- macOS with M1 chips could not run binaries compiled by the compiler.

> WIP

## Conclusion

We've hit the great v0.4 milestone of Pen. New language syntax, standard packages, other functionalities

Thank you for reading this article! And if you are interested in [Pen][pen], please [install](https://pen-lang/introduction/install.html), try it out, and give some feedback!

<!--
Therefore, although every function is asynchronous and preemptible, it doesn't require any CPU architecture-specific implementation of context switches. They are simply `ret` instructions.
-->

[pen]: https://pen-lang.org
[perceus]: https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/
