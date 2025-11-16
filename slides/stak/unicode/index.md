<!--
class: invert
-->

# Unicode in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

November 15, 2025

---

# Contents

- Stak Scheme
- Progress
  - Backtrace on errors
  - Unicode in `(scheme char)`
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support R7RS-small.

---

# Progress

- Backtrace on errors
- Unicode in `(scheme char)`

---

# Backtrace on errors

- On the VM, instructions are in a linked list.
- On a call instruction, the VM calls its procedure storing a return address of the **current** call instruction.
  - We cannot go back when it points to the next instruction.

![height:300px](backtrace.svg)

---

# Example

Source code:

```scheme
(import (scheme base))

(define (foo)
  (error "Oh, no!" 42)
  #f)

(let ()
  (foo)
  #f))
```

Output:

```
Oh, no! 42 [error foo eval #f]
```

---

# Unicode in `(scheme char)`

- Stak Scheme already supports Unicode in I/O.
  - i.e. UTF-8 encoding
- Now, its `(scheme char)` library also supports Unicode.

---

# Encoding Unicode tables

- Unicode defines multiple tables for character properties.
  - e.g. categories, and case mappings
- They can be fairly large.
  - One of Stak Scheme's goals is a small implementation.
- Code point table encoding
  1. Calculate differences between rows in a table.
     - Generate small integers that are encoded into a small number of bytes in bytecode encoding.
  2. Stak Scheme calculates differences between codes in the table.

---

# Future work

> WIP
