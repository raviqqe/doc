# Implementing the Perceus reference counting GC

Reference counting (RC) has rather been a minor party to the other garbage collection (GC) algorithms in functional programming in the last decades as, for example, [OCaml][ocaml] and [Haskell][haskell] use non-RC GC. However, several recent papers, such as [Counting Immutable Beans: Reference Counting Optimized for Purely Functional Programming][immutable beans] and [Perceus: Garbage Free Reference Counting with Reuse][perceus], showed efficiency of highly optimized RC GC in functional languages with sacrifice or restriction of some language features like circular references. The latter paper introduced an efficient RC GC algorithm called Perceus that is basically _all-in-one_ RC.

In this post, I describe my experience and some caveats about implementing and gaining benefits from the Perceus RC. I've been developing [a programming language called Pen](https://github.com/pen-lang/pen) and implemented large part of the Perceus RC there. I hope this post helps someone who is implementing the algorithm or even deciding if it's worth implementing it in their own languages.

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
- Relaxed atomic operations on references not shared by multiple threads

Due to some differences of language features between Koka and Pen, I needed to make some modifications to the algorithm. First, Pen doesn't need any complex algorithm for in-place record updates with heap reuse specialization because it has [syntax for record updates](https://pen-lang.org/references/language/types.html#records) and its lowered directly into its mid-level intermediate representation (MIR) where the RC algorithm is applied.

Secondly, although I've also implemented generic reuse of heap blocks that matches their frees and allocations in functions initially, I've reverted it back for now since I realized that it won't improve performance much in Pen because of lack of pattern matching syntax with deconstruction and another optimization of small record unboxing planned to be implemented later. In addition, the implementation doesn't include borrow inference yet as it had the least contribution to performance reported in a previous paper.

The main part of the algorithm is implemented in the source files of a compiler itself and an FFI library in Rust listed below:

- https://github.com/pen-lang/pen/blob/d44df6d9cdcbe97fcdd5ac14c4de30f4897664ff/lib/mir-fmm/src/reference_count/pointer.rs
- https://github.com/pen-lang/pen/blob/d44df6d9cdcbe97fcdd5ac14c4de30f4897664ff/lib/ffi/src/arc/arc_block.rs

### Counting back synchronized references to 0

> In this section, I use the term "synchronized" to mean "marked as shared by multiple threads." In Koka and Lean 4, they use the term "shared" to mean the same thing but I rephrased it to reduce confusion.

In the Perceus reference counting GC, memory blocks have mainly two _un-synchronized_ and _synchronized_ states represented by positive and negative counts respectively. Heap blocks are _synchronized_ before they get shared with other threads and are never reverted back to _un-synchronized_ states once they get synchronized. But you may wonder if this is necessary or not. If we have a memory block with a reference count of 1, that also means it's not shared with any other threads anymore. So isn't it possible to use a common count value of 0 to represent unique references and reduce overhead of some atomic operations potentially?

The answer is no because in that case we need to synchronize memory operations on those references _un-synchronized_ back with drops of those references by the other threads with release memory ordering. For example, let's consider a situation where a thread shares a reference with the other thread:

1. Thread A shares a reference with thread B.
1. Some computation goes on...
1. Thread B drops the reference.
1. Thread A drops the reference **and**\_ frees its inner memory block.
   - Or, thread A reuses the memory block for heap reuse optimization mentioned in the earlier section.

So if references can be _un-synchronized_ back, we always need to use atomic operations with acquire memory ordering at the point (4) above to make all side effects performed by thread B at the point (3) visible for thread A. Otherwise, thread A might free or rewrite memory locations thread B is trying to read! So in the result, we are rather increasing the overhead of atomic operations for references never _synchronized_ before.

## Benefitting from the algorithm

In general, to get most out of heap reuse in the Perceus algorithm, we need to write codes so that data structures filled with old data are updated with small portion of new data. Pen's compiler previously had a performance bug where a relatively old data structure was merged into a new one of the same type. As a result, the code to merge two pieces of data was taking almost double in time although the logic was semantically correct.

### Recursive data types

When your language has record types and syntax for record field access, things might be a little complex. Let's think about the following pseudo code where we want to update a recursive data structure of type `A` in place (The code is written in [Elm](https://elm-lang.org/) but assume that we implemented it with Perceus.):

```elm
type alias A =
  { x : Maybe A
  , y : Int
  }

f : A -> A

-- From here, assume that we are in a function scope rather than a module scope.
foo : A
foo = { x = Nothing, y = 42 }

bar : A
bar =
  { foo |
    x = case foo.x of
      Nothing -> Nothing
      Just x -> f x
  }
```

At the line of `Just x -> f x`, the program applies a function `f` to a field value `x` which originates from `foo`. However, at this point of the function application, we are still keeping the record value `foo` itself and the value of `x` has two references! Therefore, heap reuse specialization (i.e. in-place record update) cannot be applied there. In order to update the value of `x` in place instead, we need to rather deconstruct `foo` into its inner values first as follows.

```elm
bar =
  let { x, y } = foo
  in
    { x =
      case x of
        Nothing -> Nothing
        Just x -> f x
    , y = y
    }
```

Note that, even if languages do not support record deconstruction, dropping fields containing its own types is possible for self-recursive types in most cases in practice because otherwise such types' values cannot exist at runtime unless they are dynamically generated in functions or thunks in those fields.

When I look at [the Koka's documentation](https://koka-lang.github.io/koka/doc/book.html#sec-copying), it seems to support record types but I couldn't find out how it solves this case yet. It's also an option to expose the compiler's details and allow annotations to enforce in-place updates for end-users while it might not be the best option in a long term.

## Conclusion

In my experience so far, implementing the Perceus algorithm appears to be fairly straightforward compared with the other non-RC GC algorithms while there are some points to be careful about especially if you are not familiar with low-level concurrency and atomic instructions.

I'm pretty happy having the algorithm implemented in my language and seeing it performing well despite its simple implementation. The Perceus RC can be a game changer in functional programming as it outperforms traditional GC on several common patterns in functional programming. However, it's definitely not for everyone and most likely affects language design.

Finally, thanks for reading! I would appreciate your feedbacks on this post and [the Pen programming language][pen]. The language's new release has been blocked by [LLVM 14 adoption in Homebrew](https://github.com/Homebrew/homebrew-core/pull/97618) but the ticket had some progress in the last few weeks. So I can probably release v0.4 of it soon...

[pen]: https://github.com/pen-lang/pen
[ocaml]: https://ocaml.org/
[haskell]: https://www.haskell.org/
[immutable beans]: https://arxiv.org/abs/1908.05647
[perceus]: https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/
