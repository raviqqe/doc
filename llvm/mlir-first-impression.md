# MLIR first impression

## Overview

- Multi-level IR compiler framework
- Provides IR for high-level programming languages
- Core language + dialects
  - You can define your own dialect!

## Core concepts

- Operations
  - Module (`builtin.module`)
  - Function (`func.func`)
- Regions
  - SSA CFG region
  - Graph region
- Blocks
  - A list of operations (instructions)
  - Or, a set of operations (nodes)
- Dialects
  - e.g. `builtin`, `llvm`, `scf`, `gpu`, `async`

## Smell of maching learning

- `bfloat16` in the `builtin` dialect.
- Linear algebra-related dialects
  - e.g. `linalg`, `tensor`, `sparse_tensor`, `quant`
- Tensor shape inference
  - e.g. `tensor<$1x$2xf32> * tensor<$2x$3xf32> = tensor<$1x$3xf32>`
