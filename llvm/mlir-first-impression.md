# MLIR first impression

## Overview

- Multi-level IR compiler framework
- Provides IR for high-level programming languages
- Core language + dialects
  - You can define your own dialect!

## Core concepts

- Operations
  - Operations are statically and fully typed.
  - e.g. module (`builtin.module`,) function (`func.func`)
- Regions
  - SSA CFG region
  - Graph region
- Blocks
  - A list of operations (instructions)
  - Or, a set of operations (nodes)
- Attributes
  - Given to operations as name-value pairs
  - Attributes are also typed.
- Types

### Dialects

- Defines custom operations, types, and attributes.
- e.g. `builtin`, `llvm`, `scf`, `gpu`, `async`

### Passes

- Transformation
  - e.g. canonicalization, SCE, DCE, function inlining, SCCP, etc.
- Conversion
  - Converts a dialect to another
  - e.g. `scf` to `llvm`
- Others
  - Dialect-specific passes

## Smell of machine learning

- `bfloat16` in the `builtin` dialect.
- Linear algebra-related dialects
  - e.g. `linalg`, `tensor`, `sparse_tensor`, `quant`
- Tensor shape inference
  - e.g. `tensor<$1x$2xf32> * tensor<$2x$3xf32> = tensor<$1x$3xf32>`

## References

- [MLIR documentation](https://mlir.llvm.org/docs/)
