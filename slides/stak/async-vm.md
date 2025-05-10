# Asynchronous operations in the Stak Scheme virtual machine

[@raviqqe](https://github.com/raviqqe)

May 10, 2025

---

# Contents

- Stak Scheme
- Progress
  - Rust error handling in Scheme
  - Asynchronous operations
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support the R7RS-small standard.
- Forked from [Ribbit Scheme](https://github.com/udem-dlteam/ribbit)

---

# Progress

- Rust error handling in Scheme
- Asynchronous operations in virtual machines

---

# Rust error handling in Scheme

- Stak Scheme could handle errors from Rust.
  - But it was in a very limited way.
  - Rust primitives **return** error values.
  - If we check returned values and they are error values, we throw the errors.
  - Otherwise, we simply return them.
- In the new implementation of error handling, Rust primitives return `Result<V, E>` where `E` is an arbitrary error type.
  - The virtual machine now capures such errors if error handlers are assigned in the Scheme side.

---

# Rust error handling in Scheme

- [VM implementation in Rust](https://github.com/raviqqe/stak/blob/d53e20ae2bca0a334fcc4513e54133a71279be99/vm/src/vm.rs#L100)
- [Error handler implementation in Scheme](https://github.com/raviqqe/stak/blob/d53e20ae2bca0a334fcc4513e54133a71279be99/prelude.scm#L1779)

---

# Asynchronous operations

---

# Future work

- `include` syntax

---

# Summary

- Building a bytecode encoder is fun! 😃
