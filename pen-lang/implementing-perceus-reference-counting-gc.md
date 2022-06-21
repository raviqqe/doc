# Implementing the Perceus reference counting GC

Reference counting (RC) has been less popular than the other garbage collection (GC) algorithms in functional programming in the last decades as, for example, [OCaml][ocaml] and [Haskell][haskell] use non-RC ones. However, several recent papers, such as [Counting Immutable Beans: Reference Counting Optimized for Purely Functional Programming][immutable beans] and [Perceus: Garbage Free Reference Counting with Reuse][perceus], showed efficiency of highly optimized RC GC in functional languages with sacrifice or restriction of some language features like circular references. The latter paper introduced an efficient RC GC algorithm called Perceus that is basically _all-in-one_ RC.

In this post, I describe my experience and some caveats about implementing and utilizing benefits of the Perceus RC. I've been developing [a programming language called Pen](https://github.com/pen-lang/pen) and implemented part of the Perceus RC there. I hope this post helps someone who is implementing the algorithm or even deciding if it's worth implementing it in their own languages.

## Overview of Perceus

The Perceus reference counting algorithm is a thread-safe ownership-based reference counting algorithm with several optimizations:

- Heap reuse on data structure construction and deconstruction (pattern matching)
- Heap reuse specialization (in-place updates of data structures)
- Non-atomic operations or atomic operations with relaxed memory ordering for heap blocks not shared by multiple threads
- Borrow inference to reduce reference count operations

By implementing all of those optimizations in [the Koka programming language](https://github.com/koka-lang/koka), they achieved GC overhead much less and execution time faster than the other languages including OCaml, Haskell, and even C++ in several algorithms and data structures that frequently keep common sub-structures of them, such as red-black trees. For more information, see [the latest version of the paper][perceus].

## Implementing the algorithm

What I've implemented so far in Pen are two core functionalities of the Perceus algorithm:

- In-place updates of records on heap
  - This corresponds to heap reuse specialization described above.
- Relaxed atomic operations on reference counts

Due to some language feature differences between Koka and Pen, I needed to make some modifications to the algorithm. First, Pen doesn't need any complex algorithm for in-place record updates with heap reuse specialization because it has [syntax for record updates](https://pen-lang.org/references/language/types.html#records) and its lowered directly into its mid-level intermediate representation (MIR) where the RC algorithm is applied.

Secondly, although I've also implemented generic reuse of heap blocks that matches their frees and allocations in functions initially, I've reverted it back for now because I realized that it won't improve performance much in Pen because of lack of pattern matching syntax in the language and other optimization of small record unboxing that is going to be implemented soon. In addition, the implementation doesn't include borrow inference yet as it had the least contribution to performance in a previous paper.

The main part of the algorithms is implemented in the source files below of a compiler itself and an FFI library in Rust:

- https://github.com/pen-lang/pen/blob/d44df6d9cdcbe97fcdd5ac14c4de30f4897664ff/lib/mir-fmm/src/reference_count/pointer.rs
- https://github.com/pen-lang/pen/blob/d44df6d9cdcbe97fcdd5ac14c4de30f4897664ff/lib/ffi/src/arc/arc_block.rs

### Counting back synchronized references to 0

In the Perceus reference counting GC, heap blocks are never reverted back to un-synchronized state once they get synchronized. But you may wonder if this is necessary or not.

The answer is yes.

> WIP

## Benefitting from the algorithm

In general, to get most out of heap reuse in the algorithm, you need to write your code so that data structures filled with old data get updated with small updates of new data. Pen's compiler previously had a performance bug where a relatively old data structure was merged into a new one of the same type. As a result, the code to merge two pieces of data was taking almost double in time although semantically the logic was correct.

### Recursive data types

When your language has record types and has syntax for record field access, things might be a little complex. Let's think about the following pseudo code where we want to update recursive data structure of type `A` in place (The code is written in [Elm](https://elm-lang.org/) but assume that we implemented it with Perceus.):

```elm
type alias A = { x: Maybe A, y: Int }

f : A -> A

-- From here, assume we are in a function rather than a module scope.
foo : A
foo = { x = Nothing, y =  42 }

bar : A
bar = {
    foo |
    x = case foo.x of
      Nothing -> Nothing
      Some x -> f x
  }
```

At the line of `Some x -> f x`, the program applies a function `f` to a field value `x` which originates from `foo`. However, at this point of the function application, we are still keeping the record value `foo` itself and the value of `x` has two references! In order to update the value of `x` in place, you need to rather deconstruct `foo` into its inner values as follows.

```elm
bar =
  let { x, y } = foo
  in
    { x = case x of
          Nothing -> Nothing
          Some x -> f x
    , y = y
    }
```

Note that dropping fields containing its own types is possible for self-recursive types in most cases in practice because otherwise such types' values cannot exist at runtime unless they are dynamically generated in functions or thunks in the fields.

When I look at [the Koka's documentation](https://koka-lang.github.io/koka/doc/book.html#sec-copying), it seems to have record type support but I couldn't figure out how it solves this case yet. It's also an option to expose the compiler's details and allow annotations to enforce in-place updates for end-users while it might not be the best option in a long term.

## Conclusion

In my experience so far, implementing the Perceus algorithm appears to be fairly straightforward compared with the other non-RC GC algorithms while there are some points to be careful about especially if you are not familiar with low-level concurrency and atomic instructions.

I'm pretty happy having the algorithm implemented in my language and seeing it performing well despite its simple implementation. The Perceus RC can be a game changer in functional programming as it outperforms traditional GC's on several common patterns in functional programming. However, it's definitely not for everyone and most likely affects language design.

Finally, thanks for reading! I would appreciate your feedbacks on this post and [the Pen programming language][pen]. The language's new release has been blocked by [LLVM 14 adoption in Homebrew](https://github.com/Homebrew/homebrew-core/pull/97618) but the ticket had some progress in the last few weeks. So I can probably release v0.4 of it soon...

[pen]: https://github.com/pen-lang/pen
[ocaml]: https://ocaml.org/
[haskell]: https://www.haskell.org/
[immutable beans]: https://arxiv.org/abs/1908.05647
[perceus]: https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/
