# `eval`ing macros in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

June 2, 2024

---

# Contents

- Stak Scheme
- Implementation in Stak Scheme
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support R7RS-small.

---

# Macros in `eval`

- Macros in Scheme are expanded at compile time.
- We need to encode macros.

---

# Future work

- Deduplication of codes between a compiler and the `(scheme eval)` library

---

# Summary

- Building `eval` is fun!
