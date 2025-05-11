# Asynchronous operations in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

May 10, 2025

---

# Contents

- Stak Scheme
- Progress
  - Rust error handling in Scheme
  - The self-embedding compiler
  - Asynchronous operations
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

- Rust error handling in Scheme
- Asynchronous operations in virtual machines

---

# Rust error handling in Scheme

- Stak Scheme could handle errors from Rust.
  - But it was in a very limited way.
  - Rust primitives **return** error values.
  - If we check returned values and they are error values, we throw the errors in Scheme.
  - Otherwise, we simply return the values again in Scheme.
- In the new implementation of error handling, Rust primitives return `Result<V, E>` where `E` is an arbitrary error type.
  - The virtual machine captures such errors if error handlers are assigned in the Scheme side.
  - Then, it continues execution from the points of the error handlers.

---

# Rust error handling in Scheme

- [VM implementation in Rust](https://github.com/raviqqe/stak/blob/d53e20ae2bca0a334fcc4513e54133a71279be99/vm/src/vm.rs#L100)
- [Error handler implementation in Scheme](https://github.com/raviqqe/stak/blob/d53e20ae2bca0a334fcc4513e54133a71279be99/prelude.scm#L1779)

---

# The self-embedding compiler

- The Stak Scheme compiler compiles itself to embed it into the `(scheme eval)` library while compiling given source codes.
- [The article about it](https://raviqqe.com/doc/posts/stak/inception/)

---

# Asynchronous operations in virtual machines

- The Stak Scheme virtual machine now handles asynchronous operations.
- Functions asynchronous potentially are marked with [the `winter-maybe-async` crate](https://crates.io/crates/winter-maybe-async).
- The current limitation is that it cannot make asynchronous and synchronous APIs coexist...
  - [Feature unification | The Cargo book](https://doc.rust-lang.org/nightly/cargo/reference/features.html#feature-unification)

---

# Demo

- [The REPL on browser thing](https://raviqqe.com/stak/)

---

# Future work

- `case-lambda` syntax
- `define-library` syntax in an interpreter
- `include` syntax

---

# Summary

- Building a bytecode encoder is fun! 😃
