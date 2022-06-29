# Announcing the Pen programming language v0.4

I've been working on [a new programming language called Pen][pen] for almost a year. And I'm excited to announce [its v0.4 release][pen-v0-4] here!

## Introduction

The Pen programming language is a programming language for scalable software development. It aims to make development of large-scale applications easy and low-cost by focusing on **maintainability** and **portability** of software. Programs written in Pen should be simple, testable, and flexible against changes.

## Changes in v0.4

Since a v0.4 release of the language had been blocked by the [LLVM 14](https://releases.llvm.org/14.0.0/docs/ReleaseNotes.html) release, it includes many new features in the language and complementary tools. Here, I've listed some major features of them.

### The Perceus reference counting GC

One of the biggest changes in a compiler is adoption of the Perceus reference counting GC.

### `go` expression

### LLVM upgrade to 14

As I mentioned above, LLVM was upgraded to 14 in the new release, which fixed many bugs including the ones related to [tail call optimization](https://github.com/raviqqe/llvm-tail-call-opt-bug). Examples of fixed bugs are:

- Certain operations on the built-in map type led to segmentation faults.
- `pen test` command failed on macOS with multiple tests.
- macOS with M1 chips could not run binaries compiled by the compiler.

## Conclusion

Thank you for reading this article! And if you are interested in [the Pen programming language](), please [install](https://pen-lang/introduction/install.html) and try it out!

<!--
Therefore, although every function is asynchronous and preemptible, it doesn't require any CPU architecture-specific implementation of context switches. They are simply `ret` instructions.
-->

[pen]: https://pen-lang.org
[pen-v0-4]: https://github.com/pen-lang/pen/releases/tag/v0.4.0
