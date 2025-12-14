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

### Bytecode sizes (bytes)

| Source file   | Before (v0.11.9) | After (v0.11.11) | Saving rate |
| ------------- | ---------------: | ---------------: | ----------: |
| `run.scm`     |            53883 |            41092 |       0.237 |
| `repl.scm`    |            53506 |            40740 |       0.239 |
| `compile.scm` |            70501 |            53402 |       0.243 |

---

## LZSS algorithm

- LZSS is a _superior_ run-length encoding (RLE).
- It can encode repetitions of multiple bytes.

Uncompressed data:

```text
         current
            | Look back by 3 bytes.
            | Repeat 8 bytes.
            v
|x|y|z|a|b|c|a|b|c|a|b|c|a|b|d|e|f|...
```

Compressed data:

```text
|x|y|z|a|b|c|3,8|d|e|f|...
```

---

## Future work

- Tree-based vector
- Soft float
