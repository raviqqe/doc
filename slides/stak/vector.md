# Vectors in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

February 7, 2026

---

# Contents

- Stak Scheme
- `eval` in R7RS
- Implementation in Stak Scheme
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support R7RS-small.

---

# Progress

- `O(log(n))` vectors

---

# Vectors in R7RS

- Two vector types are defined in R7RS; `vector` and `byte-vector`
- Their operations seem to expect the _raw_ vectors.
  - `vector-set!`: destructive update of a vector element.
  - `vector-copy!`: destructive copy of elements from another vector.
  - `vector-append`: persistent appending of elements in multiple vectors.
    - Allocates a **new** vector!
- The situation is similar for `byte-vector`.
- The philosophy appears to be providing very basic data structures but not high level abstractions.

---

# Options in Stak Scheme

1. Raw vectors
   - We implement the raw vectors as real contiguous vectors in heap.
   - This is a bit difficult due to the current design of the VM focused in simplicity.
2. Radix vector
   - A vector based on the radix tree.
   - Each node can be a raw vector.
3. RRB vector
   - The state of the art data structure of persistent vectors
   - Every operation is `O(log(n))`.
     - Including concatenation, splits, and slicing.
   - Relaxed nodes require index arrays.

---

# Options in Stak Scheme

Stak Scheme took the radix tree of option 2.

## Why?

- RRB vectors' optimality is very attractive.
- But with the costs of the algorithm and data structure complexity.
  - Especially, the additional index arrays do not seem to fit in the design of the VM.
  - In the worst case, it doubles the memory usage of vectors.

---

# Implementation

- Each node is a list of elements.
  - The VM of Stak Scheme does not have any contiguous memory block but only cons cells.
- A slightly high branching factor of 64.
  - 32 is a popular choice for the cache line size?
  - But anyway, nodes are lists in Stak Scheme...
- Practically, the complexity of element access is `O(1)`.

---

# Is it actually fast???

- Baseline: `list`
  - `make-list`, `list-ref`, and `list-set!`
- Relative speed-up

| Elements | vector |
| -------: | -----: |
|       10 |   0.99 |
|      100 |   1.00 |
|     1000 |   0.96 |
|     2000 |   0.98 |
|     5000 |   1.52 |
|    10000 |   3.50 |

---

# Future work

- Soft float
- Rust integration

---

# Summary

- Building `eval` is fun!
