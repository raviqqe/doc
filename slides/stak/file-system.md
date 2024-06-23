# File system in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

June 23, 2024

---

# Contents

- Stak Scheme
- File system in R7RS
- Implementation
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
- The compiler is written in Scheme.
- The VM is written in Rust.
- It aims to support R7RS-small.

---

# File system in R7RS

- Generic I/O
  - Port operations: `input-port?`, `output-port?`, `call-with-port`
  - Read operations: `read-u8`, `read-string`, `read`
  - Write operations: `write-u8`, `write-string`, `write`

---

# Implementation

- Directly talks to libc.

```scheme
(define-record-type port
  (make-port* read write close last-byte)
  port?
  (read port-read)
  (write port-write)
  (close port-close)
  (last-byte port-last-byte port-set-last-byte!))
```

---

# Future work

- Efficient Scheme file compilation in Rust projects.
- More R7RS compatibility
