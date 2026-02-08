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

```scheme
(eval <expr-or-def> <environment>)
```

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

---

# Is it actually fast???

- Baseline: `list`
  - `make-list`, `list-ref`, and `list-set!`
- Branching factor: 64
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
