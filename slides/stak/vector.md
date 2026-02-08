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

1. Implementing raw vectors
   - We implement the raw vectors as real contiguous vectors in heap.
   - This is a bit difficult due to the current design of the VM focused in simplicity.
2. Implementing radix vectors
3. Implementing vectors

---

# Future work

- Soft float
- Rust integration

---

# Summary

- Building `eval` is fun!
