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

- Macros in Scheme can be expanded at compile time.
  - Stak's compiler does that.
- No data of macros at runtime!
- We need to encode macros into bytecodes.

```

```

---

# Implementation of macros in `eval`

---

# Future work

- Deduplication of codes between a compiler and the `(scheme eval)` library

---

# Summary

- Building macros in `eval` is fun!
