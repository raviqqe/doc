# Yak shaving in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

August 17, 2025

---

# Contents

- Stak Scheme
- Progress
  - Library system in `eval` procedure
  - `case-lambda` syntax
- Memory unsafety in virtual machines
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support the R7RS-small standard.
- Forked from [Ribbit Scheme](https://github.com/udem-dlteam/ribbit)

## References

- [GitHub](https://github.com/raviqqe/stak)
- [Website](https://raviqqe.com/stak)

---

# Progress

- The (almost) full R7RS-small compatibility
- The website optimization

---

# The (almost) full R7RS-small compatibility

- Stak Scheme is (almost) fully compatible with R7RS-small.
  - Other than the `include` and `include-ci` syntaxes...
  - Stak Scheme does not implement the full numeric tower either.

---

# The `include` syntax

- The `include` syntax allows
- Its argument must be "a string literal."

---

# Future work

- Unicode support in the `(scheme char)` library
- `close` primitive optimization
- Closure space safety
- Stack trace

---

# Summary

- Building Scheme is fun! 🥳
