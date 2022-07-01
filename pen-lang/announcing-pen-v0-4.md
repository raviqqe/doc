# Announcing the Pen programming language v0.4

[The Pen programming language][pen] is a new parallel, concurrent, statically typed, functional programming language. I'm excited to announce [its v0.4 release](https://github.com/pen-lang/pen/releases/tag/v0.4.0) here!

I've been working on this programming language project for almost a year. Recently, we've released its new version with new syntax constructs, standard packages, [Rust](https://www.rust-lang.org/) FFI (Foreign Function Interface) and complementary tools like formatter and documentation generator.

In this post, I would like to introduce [the Pen programming language][pen], and describe its current status and new features included in the latest release.

## Install

To try out he Pen, you can use [Homebrew](https://brew.sh) to install it on Linux, macOS, and [WSL](https://docs.microsoft.com/en-us/windows/wsl/install) on Windows.

```sh
brew install pen-lang/pen/pen
```

For more information on how to write and build programs in Pen, see [Getting started](https://pen-lang.org/introduction/getting-started.html).

## Introduction

The Pen programming language is a general-purpose, strongly and statically typed, functional programming language with automatic precise memory management based on ownerships. And it's also made for _scalable_ software development. It aims to make development of large-scale applications easy and scalable by focusing on software **maintainability** and **portability**. Programs written in Pen should be simple, testable, and flexible against changes in the real world.

The language that influenced Pen the most is [Go](https://go.dev/). You can also call Pen a functional descendant of Go with focus on application programming. Pen shares the same goal of Go like simplicity, reliability, efficiency, etc. Though, Pen explicitly excludes system programming from its domain to pursue further simplicity, portability, and developer productivity.

## Current status

Currently, the language is at the stage of syntax finalization. The last missing piece of the syntax constructs is [generic built-in functions](https://github.com/pen-lang/pen/discussions/1083) similar to Go's. They are expected to be called directly and behave similarly to built-in operators rather than normal functions. By introducing them, Pen can implement more built-in operations without increasing the language's complexity due to addition of syntax constructs. Also, that makes Pen more flexible as a language against requirement changes in the future.

## Changes in v0.4

Since a v0.4 release of the language is dependent on and had been blocked by a release of [LLVM 14](https://releases.llvm.org/14.0.0/docs/ReleaseNotes.html), it includes many new features in the language and complementary tools. Here are major ones of them.

### The Perceus reference counting GC

One of the biggest changes in a compiler is implementation of [the Perceus reference counting GC][perceus] (Garbage Collection.) It is one of the state-of-the-art GC algorithms for functional programming languages. Compared with non-reference-counting GCs which are more popular in other functional programming languages, such as OCaml and Haskell, its implementation is relatively simple even though it performs comparably to them as described in the paper. Also, adoption of such a reference counting GC makes programs written in Pen more portable even into WASM without any modification. This is because other GC methods commonly require full view of memory to track which memory locations are still reachable. Checking the entire memory including stacks at runtime requires target-specific logic or is impossible on some targets like WASM. I described more details on implementation and performance benchmarks of the Perceus reference counting algorithm in [another post](https://dev.to/raviqqe/implementing-the-perceus-reference-counting-gc-5662).

### Rust FFI

[A `pen-ffi` crate](https://crates.io/crates/pen-ffi) is a Rust FFI library for Pen. Thanks to both languages' semantics based on ownerships, they can interoperate with each other very easily. The new standard packages included in this release like `Http` and `Sql` packages are actually simple wrappers of third-party crates in Rust. Using the Rust FFI library, you can also write your own packages in Pen while utilizing existing resources written in Rust and benefiting from its growing ecosystem.

### `go` expression

It's still experimental but we've introduced the first piece of parallel and concurrent computation primitives, `go` expression. By using the `go` expression originating from Go, you can delegate heavy computation, slow I/O, or any other computation you want to perform concurrently to the other execution contexts like threads.

```pen
future = go \() number {
  # Some heavy computation...
  x + y
}
```

### Formatter

Now, Pen is equipped with its official formatter command of `pen format`. This command formats each source file or ones in a directory of the entire package at once. It can also receive source from standard inputs so that it integrates with existing editors and IDEs easily.

Similarly to Go's `gofmt` and differently from some other formatters like `rustfmt` in Rust, the source formatter of Pen does not define any canonical form of source codes given the same tokens. But it rather tries to align indents, and add and remove spaces and newlines given hints extracted from the original source codes so that developers can still control their source codes to make them look beautiful in their own contexts. For more information, see `pen format --help`.

### Documentation generator

The new release also includes a new `pen document` command that generates a documentation file of a package in Markdown. To generate the documentation, you can simply run `pen document` with some options, such as a package name and URL, in a package directory. For more information, see `pen document --help`.

### `Http` and `Sql` standard packages

In addition to new functions and types in the `Core` and `Os` standard packages, we've added `Http` and `Sql` standard packages. As their names suggest, the `Http` package provides HTTP server and client and the `Sql` package provides a `Sql` client.

Because those packages are dependent on the third-party crates ([`hyper`](https://github.com/hyperium/hyper) and [`sqlx`](https://github.com/launchbadge/sqlx) respectively) in Rust, they are planned **not** to be included the default installation bundle of the language. But they are likely to be separated into different Git repositories.

### LLVM upgrade to 14

As I mentioned above, LLVM was upgraded to 14 in the new release, which fixed many bugs including the ones related to [tail call optimization](https://github.com/raviqqe/llvm-tail-call-opt-bug). Examples of fixed bugs are:

- Certain operations on the built-in map type led to segmentation faults.
- `pen test` command failed on macOS with multiple tests.
- macOS with M1 chips could not run binaries compiled by the compiler.

Now, most programs written in Pen should work also on macOS with M1 chips as well as on x86-64 chips. Although there is a plan to support macOS as the first tier platform, we can't guarantee that Pen works properly there due to limitation of CI infrastructure.

## What's next?

There are quite a few features planned for the next version of Pen including:

- Generic built-in functions
- Proper implementation of the C calling convention for FFI
- More functionalities in standard packages

The C calling convention is already implemented. However, the current naive implementation is inefficient and sometimes requires heap allocations. By implementing it properly, C/Rust functions can be called without unnecessary overheads.

## Conclusion

We've hit the great v0.4 milestone of Pen! It contains new syntax, standard packages, and other complementary functionalities.

Thank you for reading this article! And if you are interested in [Pen][pen], please [install](https://pen-lang/introduction/install.html), try it out, and give some feedback!

<!--
Therefore, although every function is asynchronous and preemptible, it doesn't require any CPU architecture-specific implementation of context switches. They are simply `ret` instructions.
-->

[pen]: https://pen-lang.org
[perceus]: https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/
