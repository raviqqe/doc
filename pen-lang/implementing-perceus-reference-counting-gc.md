# Implementing the Perceus reference counting GC

Reference counting (RC) has been less popular than the other garbage collection (GC) algorithms in functional programming in the last decades as, for example, [OCaml][ocaml] and [Haskell][haskell] use non-RC ones. However, several recent papers, such as [Counting Immutable Beans: Reference Counting Optimized for Purely Functional Programming][immutable beans] and [Perceus: Garbage Free Reference Counting with Reuse][perceus], showed efficiency of RC GC in functional languages while sacrificing or limiting some features like circular references.

In this post, I would like to describe some caveats about implementing and utilizing benefits of the Perceus RC. I've been developing a programming language called [Pen](https://github.com/pen-lang/pen) and implemented part of the Perceus RC there. I hope this post helps someone who is implementing the algorithm or even deciding if it's worth implementing it in their own languages.

## Overview of Perceus

> WIP

## Implementing the algorithm

What I've implemented so far in [Pen][pen] are two core functionalities of the Perceus algorithm:

- In-place updates of record type values
- Heap reuse on updates

Although I've also implemented generic heap reuse for heap blocks initially, I've reverted it back for now because I realized that it won't improve performance much due to some language differences between Pen and the languages in the paper.

The main part of the algorithms are implemented in the files below for a compiler itself and a FFI library:

- https://github.com/pen-lang/pen/blob/d44df6d9cdcbe97fcdd5ac14c4de30f4897664ff/lib/mir-fmm/src/reference_count/pointer.rs
- https://github.com/pen-lang/pen/blob/d44df6d9cdcbe97fcdd5ac14c4de30f4897664ff/lib/ffi/src/arc/arc_block.rs

### Counting back synchronized references to 0

In the Perceus reference counting GC, references are never reverted back to un-synchronized state once they get synchronized. But you may wonder if this is necessary or not.

The answer is yes.

> WIP

## Benefitting from the algorithm

> WIP

### Recursive data types

> WIP

Note that dropping fields of its own types is always possible in practice because otherwise such types' values do not exist at runtime.

## Conclusion

In my experience so far, implementing the Perceus algorithm is quite straightforward compared with implementing the other GC algorithms while there are some points to be careful about especially if you are not faimiliar with low-level concurrency and atomic instructions.

The Perceus RC can be a game changer in functional programming and outperform traditional GC's in several programming patterns. But it's definitely not for everyone and most likely affect your language design.

Finally, I would appreciate if you give feedback on this post and [the Pen programming language][pen]. Thanks for reading!

[pen]: https://github.com/pen-lang/pen
[ocaml]: https://ocaml.org/
[haskell]: https://www.haskell.org/
[immutable beans]: https://arxiv.org/abs/1908.05647
[perceus]: https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/
