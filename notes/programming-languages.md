# Programming languages

## Languages

- [Effekt](https://effekt-lang.org/)
- [Elixir](https://elixir-lang.org/)
- [Elm](https://github.com/elm)
- [Expresso](https://github.com/willtim/Expresso)
- [Flix](https://flix.dev/)
- [Gluon](https://github.com/gluon-lang/gluon)
- [IntercalScript](https://github.com/Storyyeller/IntercalScript)
  - Case objects are similar to polymorphic variants in OCaml.
- [Koka](https://github.com/koka-lang/koka)
- [Lean 4](https://github.com/leanprover/lean4)
- [Lobster](https://github.com/aardappel/lobster)
- [Lua](https://www.lua.org)
- [Nim](https://github.com/nim-lang/Nim)
  - Its garbage collection with deferred reference counting is interesting.
- [OCaml](https://github.com/ocaml/ocaml)
- [Scala](https://www.scala-lang.org/)
- [Swift](https://swift.org/)
- [Uiua](https://github.com/uiua-lang/uiua)
- [Unison](https://www.unisonweb.org/)
- [Yatima](https://github.com/yatima-inc/yatima)
- [Ruby](https://www.ruby-lang.org)
  - [Parallel and Thread-Safe Ruby at High-Speed with TruffleRuby](https://speakerdeck.com/eregon/parallel-and-thread-safe-ruby-at-high-speed-with-truffleruby)
- [Scheme](https://www.scheme.org/)
  - [R7RS](https://small.r7rs.org/attachment/r7rs.pdf)
- [Racket](https://racket-lang.org/)

### Go

- [Go](https://github.com/golang/go)
  - The package namespace system like Go works only with object-oriented programming as classes create sub-namespaces.
- [Channels - A Tour of Go](https://tour.golang.org/concurrency/2)
- [`DQNEO/babygo`](https://github.com/DQNEO/babygo)

### Haskell

- [Glasgow Haskell Compiler](https://github.com/ghc/ghc)
- [The Spineless Tagless G-machine](https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf)

### Python

- [CPython](https://github.com/python/cpython)
- [PyPy](https://www.pypy.org/)
- [Cinder](https://github.com/facebookincubator/cinder)
- [Mojo](https://www.modular.com/mojo)

### Scheme

- [R7RS](https://small.r7rs.org/attachment/r7rs.pdf)
- [Racket](https://racket-lang.org/)
- [scheme.org](https://www.scheme.org/)
- [SRFI](https://srfi.schemers.org/)
- [R7RS pico](https://github.com/jrincayc/r7rs-pico-spec)
- [Bottom Scheme](https://github.com/johnwcowan/r7rs-work/blob/master/BottomScheme.md)
- [Scheme 4 In One File](https://github.com/false-schemers/s4iof)

### TypeScript

- [TypeScript](https://github.com/microsoft/TypeScript)
- [Deno](https://deno.land/)

### Ruby

- [CRuby](https://www.ruby-lang.org)
- [Parallel and Thread-Safe Ruby at High-Speed with TruffleRuby](https://speakerdeck.com/eregon/parallel-and-thread-safe-ruby-at-high-speed-with-truffleruby)

### Rust

- [Guide to Rustc Development](https://rustc-dev-guide.rust-lang.org/)
- [Reading Arc in Rust (Japanese)](https://qiita.com/qnighy/items/35db580a139d21f38410)

## Technologies and concepts

### Language design

- [プログラミング言語の未来はどうなるか](https://keens.github.io/blog/2021/01/04/future_of_proguramming_languages/)
- [Robert Virding - On Language Design (Lambda Days 2016)](https://www.youtube.com/watch?v=f3rP3JRq7Mw)
- [The Rise of "Worse is better"](https://www.dreamsongs.com/RiseOfWorseIsBetter.html)
- [Design Criteria for Programming Languages](http://jcsites.juniata.edu/faculty/rhodes/lt/plcriteria.htm)
- [Go at Google: Language Design in the Service of Software Engineering](https://go.dev/talks/2012/splash.article)
- [Language Design](https://cs.lmu.edu/~ray/notes/languagedesignnotes/)

### Type system

- [Lecture slides of recursive types at Cornell University](https://www.cs.cornell.edu/courses/cs4110/2012fa/lectures/lecture27.pdf)
- [Algebraic subtyping](https://www.cs.tufts.edu/~nr/cs257/archive/stephen-dolan/thesis.pdf)
  - [Polymorphism, Subtyping, and Type Inference in MLsub](https://dl.acm.org/doi/10.1145/3009837.3009882)
- [Practical type inference for arbitrary-rank types](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf)
- [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://arxiv.org/abs/1306.6032)

### Effect system

- [Effects bibliography](https://github.com/yallop/effects-bibliography)
- [libhandler](https://github.com/koka-lang/libhandler)

### Garbage collection

- [ラピッドイテレーションを実現するRE ENGINEの設計](https://www.docswell.com/s/CAPCOM_RandD/KQQWMK-2022-07-15-133419)

#### Reference counting

- [Counting Immutable Beans: Reference Counting Optimized for Purely Functional Programming](https://arxiv.org/abs/1908.05647)
  - [Slides](https://arxiv.org/abs/1908.05647)
- [Perceus: Garbage Free Reference Counting with Reuse](https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/)
- [Reference Counting with Frame Limited Reuse](https://www.microsoft.com/en-us/research/publication/reference-counting-with-frame-limited-reuse-extended-version/)

#### Immix GC

- [`mu/immix-rust`](https://gitlab.anu.edu.au/mu/immix-rust)
- [Immix: A Mark-Region Garbage Collector with Space Efficiency, Fast Collection, and Mutator Performance](https://dblp.org/rec/conf/pldi/BlackburnM08.html)
- [Taking Off the Gloves with Reference Counting Immix](http://users.cecs.anu.edu.au/~steveb/pubs/papers/rcix-oopsla-2013.pdf)
- [Rust as a Language for High Performance GC Implementation](http://users.cecs.anu.edu.au/~steveb/pubs/papers/rust-ismm-2016.pdf)

### Algorithms and data structures

- [Optimizing Hash-Array Mapped Tries for Fast and Lean Immutable JVM Collections](https://michael.steindorfer.name/publications/oopsla15.pdf)
- [LazyList | Scala](https://www.scala-lang.org/api/current/scala/collection/immutable/LazyList.html)
- [Foreign Function Interface | Deno](https://docs.deno.com/runtime/manual/runtime/ffi_api)

#### RRB tree

- [RRB-Trees: Efficient Immutable Vectors](https://infoscience.epfl.ch/record/169879/files/RMTrees.pdf)
  - [elm-array](https://github.com/exists-forall/elm-array)
- [Improving RRB-Tree Performance through Transience](https://hypirion.com/thesis.pdf)
  - [c-rrb](https://github.com/hypirion/c-rrb)
  - [The blog post](https://hypirion.com/musings/thesis)
  - [`arazabishov/pvec-rs`](https://github.com/arazabishov/pvec-rs)
  - [Other implementations and descriptions of RRB trees | Closure core library](https://cljdoc.org/d/org.clojure/core.rrb-vector/0.1.2/doc/other-implementations-and-descriptions-of-rrb-trees)

#### Coroutine

- [From folklore to fact: comparing implementations of stacks and continuations](https://dl.acm.org/doi/10.1145/3385412.3385994)
- [Coroutines for Go](https://research.swtch.com/coro)
- [Stackful coroutine実装まとめ](https://qiita.com/raviqqe/items/b34f067aed697a0ba63a)
- [Coroutine | Boost](https://www.boost.org/doc/libs/1_83_0/libs/coroutine/doc/html/index.html)

#### Lossless syntax tree

- [Persistence, facades and Roslyn’s red-green trees](https://ericlippert.com/2012/06/08/red-green-trees/)
- [Lossless Syntax Tree Pattern | Oil](https://github.com/oilshell/oil/wiki/Lossless-Syntax-Tree-Pattern)

### WASM

- [Emscripten](https://emscripten.org/index.html)
- [Wasmtime](https://github.com/bytecodealliance/wasmtime)
- [Cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift)
  - [Cranelift IR](https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md)
- [`bytecodealliance/wasi`](https://github.com/bytecodealliance/wasi)
- [`microsoft/mimalloc`](https://github.com/microsoft/mimalloc)
- [`bytecodealliance/wasm-micro-runtime`](https://github.com/bytecodealliance/wasm-micro-runtime)

### Concurrency and parallelism

- [Keynote: Announcing Broadway - ElixirConf EU 2019](https://www.youtube.com/watch?v=IzFmNQGzApQ)
  - Basic back pressure can be implemented as queue limits.
- [`dashbitco/broadway`](https://github.com/dashbitco/broadway)
- [Akka Stream](https://doc.akka.io/docs/akka/current/stream/index.html)
- [Runtime Support for Multicore Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/2009/09/multicore-ghc.pdf)

## Books

- [プログラマーのためのCPU入門 ― CPUは如何にしてソフトウェアを高速に実行するか](https://www.lambdanote.com/blogs/news/cpu-cpu)

## References

- [`ChessMax/awesome-programming-languages`](https://github.com/ChessMax/awesome-programming-languages)
- [`learn-anything/programming-languages`](https://github.com/learn-anything/programming-languages)
- [`Kixiron/rust-langdev`](https://github.com/Kixiron/rust-langdev)
