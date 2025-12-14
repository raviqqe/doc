<!--
theme: default
class: invert
-->

# Bytecode compression in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

December 14, 2025

---

## Contents

- Stak Scheme
- Progress
  - Bytecode compression with LZSS
- Future work

---

## Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support the R7RS-small standard.
- Forked from [Ribbit Scheme](https://github.com/udem-dlteam/ribbit)

### References

- [GitHub](https://github.com/raviqqe/stak)
- [Website](https://raviqqe.com/stak)

---

## Progress

- Bytecode compression with LZSS

---

## Bytecode compression

- Ribbit Scheme compresses its bytecode using LZSS.
  - Only in the binary (256-bit base) bytecode mode
- Stak Scheme now implements the same.
- Bytecode compression is based on the LZSS algorithm.

| Version  | Bytecode / bytes | Binary / bytes |
| -------- | ---------------: | -------------: |
| v0.11.9  |                - |              - |
| v0.11.11 |                - |              - |

---

## Foo
