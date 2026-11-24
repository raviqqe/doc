---
description: An AI security and correctness audit of Melior 0.30.0, the Rust bindings to the MLIR C API, covering memory-safety gaps reachable from its safe API, MLIR parser defects, the procedural macro that generates its dialect bindings, and its Toy compiler example.
---

# Security audit of Melior

<!-- cspell: ignore addi addui alloca canonicalizer castu clippy coiterate extui fastmath getelementptr irdl lossily melior memref miscompiled mxint repr segfaulted strided unreviewed xnone -->

- Date: 2026-11-23
- Model: Claude Opus 5.5
- Repository: [`mlir-rs/melior`](https://github.com/mlir-rs/melior)
- Version: `main` at `719ec63c2` (0.30.0)

## Summary

- Melior 0.30.0 has one Critical and 17 High findings open: 14 in `melior`, two in `melior-macro` and one in the Toy compiler `melior-kaleidoscope`. Every one of them is reproduced from safe Rust.
- The Critical finding and three High ones are defects of MLIR's parser that `Attribute::parse`, `Type::parse` and `Module::parse` reach (MEL-47, MEL-48, MEL-51, MEL-52). A few dozen bytes of IR text read memory out of bounds or crash the process, and Melior can't check for them first. Since #1044, the README and the parse functions document that untrusted text must not be parsed. A fix for MEL-47 is prepared for upstream MLIR.
- Most other High findings are about ownership of IR: safe rewriter methods free IR that live handles still reference (MEL-03), operations don't keep the owners of their operands alive (MEL-04), moves between owners are invisible to the borrow checker (MEL-05), loading a dialect after its operations exist leaves dangling operation names (MEL-49), and nested pass manager handles dangle after a run (MEL-50).
- The rest are unchecked preconditions of the C API: kind casts (MEL-07), constructors (MEL-08), type inference on unverified operands (MEL-09 and MAC-01 in generated builders), null handles (MEL-10), LLVM types without the dialect loaded (MEL-11), and generated removers of default-valued attributes (MAC-16).
- Four findings were fixed during the audit, among them the only arbitrary read from safe code (MEL-01), in which IR text chose the address and length of a string that Melior read back.

## Scope and method

- The audit covers the three packages in the repository, `melior`, `melior-macro` and `melior-kaleidoscope`, and its CI and tooling. The raw bindings in `mlir-sys` are out of scope. This report replaces the earlier version of this page at `e6407a664`. Its findings were audited again from scratch and are numbered anew.
- The first pass at `6ffe26ea6` read every line of the repository, about 30,000 lines, with separate sub-audits of `melior-macro`, `melior-kaleidoscope`, the types and attributes of `melior`, and its dialects and helpers. The second pass at `26c630dd7` read every line again, in five sub-audits that used the first report as their baseline. The final check at `719ec63c2` covered the changes since then: doc comments (#1044), dependency updates and the version bump.
- Each finding was reproduced with a safe Rust program, most of them under `#![forbid(unsafe_code)]`, unless it is marked as read. The programs ran as debug builds on macOS on Arm against Homebrew LLVM 23.1.6, which is built without assertions, so violated preconditions in MLIR are undefined behavior rather than assertion failures. Each crashing case ran in its own process, and doubtful cases ran again under Guard Malloc (`libgmalloc`).
- The status of each finding is "reproduced" when it was reproduced by the lead auditor, "reproduced by a sub-audit" when only the sub-audit of its area ran it, and "read" when it was established from the code only.
- Locations refer to `719ec63c2`.
- At `26c630dd7`, `cargo fmt --check` and `cargo clippy --all-features --all-targets -- -D warnings` were clean, and `cargo test --all-features` gave 823 passed, 3 ignored and 1 failed tests. The failure is MAC-14. At `719ec63c2`, CI passes except for the documentation deployment (INF-06).

## Severity

| Severity      | Meaning                                                                    |
| ------------- | -------------------------------------------------------------------------- |
| Critical      | Arbitrary memory reads or writes from safe code or untrusted input         |
| High          | Memory unsafety or a crash from safe code or untrusted input in common use |
| Medium        | Wrong results, aborts or contract violations with limited reach            |
| Low           | Edge cases, panics and leaks                                               |
| Informational | Documentation, tests and hygiene                                           |

## Overview

| Package               | Critical | High | Medium | Low | Informational |
| --------------------- | -------- | ---- | ------ | --- | ------------- |
| `melior`              | 1        | 14   | 19     | 20  | 7             |
| `melior-macro`        | 0        | 2    | 4      | 10  | 4             |
| `melior-kaleidoscope` | 0        | 1    | 5      | 10  | 4             |
| Repository            | 0        | 0    | 1      | 2   | 3             |

## Fixed findings

| ID     | Finding                                                                                | Fixed by                     |
| ------ | -------------------------------------------------------------------------------------- | ---------------------------- |
| MEL-01 | Dense elements with a non-builtin element type were read as strings, an arbitrary read | #1037                        |
| MEL-02 | Dense element count times width wrapped, so element accessors read out of bounds       | #1039, #1040                 |
| MEL-27 | Pattern benefits are 16 bits in MLIR, so 65535 never fired and 65537 acted as 1        | #1042 (merged through #1041) |
| MEL-39 | Rewrite patterns and pattern sets carried no context lifetime                          | #1041                        |

- MEL-01: `DenseElement for StringRef` treated every element type other than integers, indices, floats and complex numbers as string storage. In LLVM 23, `!tosa.mxint8` and `!tosa.block_scaled` use packed storage, so `string_element` reinterpreted chosen bytes as a pointer and a length. Parsing `dense<tensor<16x!tosa.mxint8> : [...]>` with 16 chosen bytes and calling `string_element(0)` returned the contents of an unrelated Rust `String`. The fix classifies elements by storage kind. A sub-audit found no remaining path to an element read that skips the storage kind and size checks.
- MEL-02: MLIR's raw buffer check multiplies in wrapping arithmetic, so `raw_buffer` of `tensor<(2^61 + 2) x i8>` with two bytes was accepted, `len()` reported 2^61 + 2, and `u8_element(8..)` returned heap bytes. Parsed attributes such as `dense<"0x…16 bytes…"> : tensor<2x144115188075855873xf64>` behaved the same. The fix computes counts and sizes with checked arithmetic, also for attributes of unknown origin.
- MEL-39: patterns now only fire on IR of their own context. Mixing two live contexts is still not detected.

## `melior`

### Critical

#### MEL-47: A parenthesized pair in a dense literal reads out of bounds

- Where: MLIR's parser, reached through `Attribute::parse`, `Type::parse` and `Module::parse` (`melior/src/ir/attribute.rs:54-61`, `melior/src/ir/type.rs:57-64`, `melior/src/ir/module.rs:31-38`).
- Status: reproduced.
- Problem: MLIR's parser accepts a parenthesized pair for any element type and checks the number of values only for complex element types. `dense<(1, 2)> : tensor<5xi32>` parses into a non-splat attribute of five elements that holds two, so `i32_element(2..)` returns heap bytes and printing shows heap values. The same holds for implicit types such as `memref.global`, type encodings and sparse literals, and string storage segfaults on any access.
- Impact: out-of-bounds reads from untrusted IR text. Melior can't detect such attributes, because the C API exposes no size of the stored data, and MLIR's printer reads the same memory before Melior is involved.
- Mitigation: #1044 documents that untrusted text must not be parsed.
- Recommendation: fix the parser upstream. A fix that rejects such literals is prepared and audited in the report "Audit of an MLIR parser fix for miscounted tensor literals" on this site.

### High

#### MEL-03: Safe rewriter methods free IR that live handles still reference

- Where: `melior/src/ir_rewriter.rs:96-142`, `:150-184`, `:262-296`; `melior/src/ir/block.rs:220-231`; `melior/src/ir/region.rs:140-151`.
- Status: reproduced. `erase_op` followed by printing the handle, `replace_op_with_values(op, &[])` on a used operation, and `insert` after the region of the insertion block was dropped all segfault without Guard Malloc.
- Problem: `erase_op`, `erase_block`, `replace_op_with_*`, `inline_block_before`, `merge_blocks` and `inline_region_before` take `Copy` handles through `&self`. The insertion point is an untracked raw pointer. Handles returned by `insert`, `clone_op` and `create_block_before` borrow the rewriter instead of the owner of the IR. Block and region iterators fetch the next element in advance.
- Impact: use-after-free from safe code.
- Recommendation: make the erasing and replacing methods `unsafe` until handles can be invalidated, check value and result counts, and tie the insertion point to a borrow of its block.

#### MEL-04: Operations don't keep the owners of their operands alive

- Where: `melior/src/ir/operation/builder.rs:55-65`; `melior/src/ir/operation/operation_like.rs:521-544`, `:597-599`; `melior/src/ir/operation.rs:87-97`.
- Status: reproduced. An operation built with an argument of a dropped block segfaults when printed.
- Problem: `add_operands(&[Value<'c, '_>])` discards the owner lifetime. `OperationMutLike for Operation<'c>` leaves the lifetime free in `set_operand`, `set_operands` and `replace_uses_of_with`, and `Operation::clone` copies operand references.
- Impact: operands dangle once the block or operation that defines them is dropped. Every hand-written and generated constructor inherits this.
- Recommendation: give operations and operation builders an owner lifetime for operands, or check for outside uses when an owner is dropped.

#### MEL-05: Moving operations and blocks between owners is invisible to the borrow checker

- Where: `melior/src/ir/operation/operation_like.rs:508-518`; `melior/src/ir_rewriter.rs:150-152`, `:186-201`.
- Status: reproduced. `first.first_operation_mut().unwrap().move_after(anchor_in_second)`, then `drop(second)`, then printing a handle borrowed from `first` segfaults.
- Problem: owner lifetimes are covariant and unify, so an operation can move into a block with another owner. Dropping that block frees the operation while handles borrowed from the original block stay usable.
- Recommendation: put moves between owners behind `unsafe` or an exclusive borrow of both owners.

#### MEL-06: Structural preconditions of block and operation manipulation are unchecked

- Where: `melior/src/ir/operation/operation_like.rs:427-429`, `:508-518`; `melior/src/ir/block/block_like.rs:103-141`; `melior/src/ir/region/region_like.rs:26-45`; `melior/src/ir_rewriter.rs:236-258`.
- Status: reproduced.
- Problem:

  | Call                                                                      | Observed                                                 |
  | ------------------------------------------------------------------------- | -------------------------------------------------------- |
  | `move_after` or `is_before_in_block` on a detached operation              | Segfault                                                 |
  | `insert_operation_after` with a reference from another block              | The other block's list is corrupted; iteration segfaults |
  | `insert_block_after` with a block of another region                       | Iteration never terminates                               |
  | `insert_operation(7, ..)` into `[a, b]`                                   | Yields `[a, c, b]`                                       |
  | `create_block_before` with a detached block (from `BlockArgument::owner`) | Segfault                                                 |

- Recommendation: check the block and parent region of both sides and the position bound, and return `Result`.

#### MEL-07: Kind casts without a check

- Where: `melior/src/ir/type.rs:142`, `:230`; `melior/src/ir/affine_expr.rs:78-98`; `melior/src/ir/affine_map.rs:166`; `melior/src/ir/attribute/integer.rs:19`; `melior/src/ir/type/mem_ref.rs:20`, `:38`; `melior/src/ir/location.rs:126-192`.
- Status: reproduced for `Type::index(..).float_width()` and `Location::unknown(..).call_site_callee()`, which segfault; the rest reproduced by a sub-audit.
- Problem: these safe functions reach an unchecked LLVM `cast<>`, or a `dyn_cast` whose null result is dereferenced:
  - `is_scalable_vector` returns `true` for `index`.
  - `AffineExpr::constant_value` on `d7 + s9` returns a heap address.
  - `AffineExpr::lhs` and `rhs` on a leaf return a handle that crashes when used.
  - `IntegerAttribute::new` with `f32` builds `1 : f32`.
  - A non-layout attribute passed as a memref layout crashes `affine_map()`.
  - Every location accessor crashes on a location of the wrong kind.
- Recommendation: guard each accessor with its predicate and return `Result` or `Option`.

#### MEL-08: Unchecked constructors build objects whose accessors corrupt memory

- Where: `melior/src/ir/type/mem_ref.rs:20-35`, `:95-107`; `melior/src/ir/affine_map.rs:55-71`, `:224-260`; `melior/src/ir/type/integer.rs:17-31`.
- Status: reproduced for the stride overflow; the rest reproduced by a sub-audit.
- Problem: `strides_and_offset` allocates one slot per dimension, and MLIR writes one per stride. With eight strides on a rank-1 memref it returns `([1], 0)` and the process dies at exit, so the overflow is silent heap corruption. A memref layout map with more dimensions than the rank makes MLIR index out of range. `AffineMap::new(context, 1, 0, &[dim 9])` followed by `is_permutation()` writes past a stack buffer. `IntegerType::new(context, 2^30 + 32)` yields a distinct type that prints as `i32`.
- Recommendation: validate stride and dimension counts in Rust, or use the checked C API, and bound integer widths to 2^24 − 1.

#### MEL-09: Result type inference runs on unverified operands

- Where: `melior/src/ir/operation/builder.rs:135-146`; `melior/src/dialect/memref.rs:259-270`.
- Status: reproduced. Twelve operations segfault in `build()` with inference and no operands, among them `shape.add`, `tosa.concat`, `vector.shuffle` and `memref.extract_strided_metadata`.
- Problem: MLIR documents that `inferReturnTypes` requires verified operands. `enable_result_type_inference` is safe, and the hand-written inference in MLIR indexes and casts operands unchecked. `memref::load(index_value, &[], location)` builds an operation whose `verify()` and `Display` segfault, and with `memref<*xf32>` the result type is null.
- Recommendation: make inference an `unsafe` opt-in, and give `memref::load` an explicit result type from `MemRefType::try_from`. MAC-01 has the same root cause in generated builders.

#### MEL-10: Null handles are wrapped in non-nullable types

- Where: `melior/src/ir/affine_map.rs:171-178`; `melior/src/context.rs:63-67`; `melior/src/ir_rewriter.rs:117-124`; `melior/src/ir/location.rs:200-202`; `melior/src/ir/module.rs:91-105`.
- Status: reproduced; each case segfaults on first use.
- Problem: the affected calls are:
  - `major_sub_map(0)`, `minor_sub_map(0)` and `major_sub_map(1 << 32)`, so a check for zero isn't enough
  - `get_or_load_dialect("unknown")`
  - `insertion_block()` and `block()` with no insertion point
  - `Location::from_attribute` with an attribute that isn't a location
  - `Module::try_from` of a `builtin.module` without a region, with a region without a block, or with two regions, followed by `body()`
- Recommendation: return `Option` or `Result` for null handles, and check the region structure in `Module::try_from`.

#### MEL-11: LLVM dialect type constructors need the dialect loaded

- Where: `melior/src/dialect/llvm/type.rs:15-64`.
- Status: reproduced from the sub-audit's program. `void` returns a null type whose `is_integer()` segfaults, and `pointer` never returns.
- Problem: on a context where the `llvm` dialect is not loaded, including one where it is registered but not loaded, the C getters reach a type uniquer with no storage. `LlvmBlockExt::alloca` and `gep` inherit this. The README lists the general hazard.
- Recommendation: load the dialect inside the constructors, or return `Result`.

#### MEL-48: Hex literals with wrapped element counts crash MLIR

- Where: MLIR's parser and passes, reached through `Attribute::parse`, `Type::parse` and `Module::parse`.
- Status: reproduced.
- Problem: the element count of a shape can overflow 64 bits, and MLIR multiplies it unchecked in builds without assertions.
  - `Attribute::parse` on `dense<"0x41"> : tensor<768614336404564651xi24>` segfaults inside the parser. It affects hex literals whose element size is not a power of two.
  - `dense<"0x0100000002000000"> : tensor<3x6148914691236517206xi32>` parses and prints as `dense<[[1, 2]]>`, because 3 × 6148914691236517206 wraps to 2. A module that extracts element `[1, 0]` of that constant passes `verify()`, and a `PassManager` running the canonicalizer then segfaults in MLIR's folder in 3 of 3 runs. Melior's own accessors are safe: `DenseElementsAttribute::try_from` rejects the attribute, because Melior computes the count with checked arithmetic (`melior/src/ir/attribute/dense_elements.rs:450-468`, `:572-582`).
- Impact: untrusted IR text crashes a consumer, also after verification. The fix prepared for MEL-47 doesn't cover it.
- Recommendation: report upstream that types whose element count overflows must be rejected; keep the warning about untrusted text.

#### MEL-49: Loading a dialect after its operations exist leaves dangling operation names

- Where: operation names are created in `melior/src/ir/operation/builder.rs:27-39` and `melior/src/rewrite_pattern.rs:81-97`; dialects are loaded explicitly in `melior/src/context.rs:63-67`, `:75-77`, `melior/src/dialect/handle.rs:107-109` and `melior/src/utility.rs:59-61`, and implicitly in `Module::parse`, `Type::parse`, `Attribute::parse`, `PassManager::run`, `melior/src/dialect/index.rs:38-53` and `melior/src/dialect/llvm/attributes.rs:38`.
- Status: reproduced; two sub-audits found it independently.
- Problem: on a context whose dialects are registered but not loaded, `OperationBuilder` builds `func.return` as an unregistered operation without an error, even when unregistered dialects are disallowed. Once the dialect loads, the name of the earlier operation is gone:

  | Trigger after building the operation                                       | Observed                                            |
  | -------------------------------------------------------------------------- | --------------------------------------------------- |
  | `get_or_load_dialect("func")` or `load_all_available_dialects()`           | The name reads as invalid UTF-8; printing segfaults |
  | `Module::parse` of text that uses the dialect                              | Segfault                                            |
  | `index::casts`, then `index::cmp`, which parses an attribute               | The first operation prints as `index.castu`         |
  | Building `"foo.bar"`, then `load_irdl_dialects` with a dialect defining it | Segfault                                            |
  | A pattern rooted at `func.call` created before loading                     | Never fires, without an error                       |

- Impact: use-after-free from safe code. The natural reaction to an error about an unregistered dialect, adding `load_all_available_dialects()`, triggers it.
- Recommendation: load the dialect of the namespace of a name in `OperationBuilder::new` and `RewritePattern::new`, as the parser does, and return an error if the operation stays unregistered while unregistered dialects are disallowed. Make `load_irdl_dialects` refuse names the context already knows, and report the mechanism upstream. It was not confirmed inside MLIR; only the freed read is observed.

#### MEL-50: Nested pass manager handles dangle after a run or pipeline parsing

- Where: `melior/src/pass/manager.rs:35-41`, `:73-83`; `melior/src/pass/operation_manager.rs:14-32`; `melior/src/utility.rs:36-55`.
- Status: reproduced.
- Problem: `OperationPassManager` is a `Copy` pointer into its parent and borrows it only shared. `PassManager::run(&self)` merges adjacent nested managers, so `any(func.func(cse),func.func(canonicalize))` becomes `any(func.func(cse,canonicalize))`, and `parse_pass_pipeline` replaces the content. Handles taken earlier stay usable, and printing one or calling `add_pass` on it segfaults.
- Recommendation: take `&mut self` in `run`, `nested_under` and pipeline parsing, drop `Copy`, and add compile-fail tests.

#### MEL-51: `Type::parse` and `Attribute::parse` crash on an empty string

- Where: `melior/src/ir/type.rs:57-64`; `melior/src/ir/attribute.rs:54-61`.
- Status: reproduced. The process exits with status 138 on a bare context and 139 with all dialects.
- Problem: `" "`, `"//"` and `"z"` return `None`, and `Module::parse(&context, "")` returns an empty module. Only the empty string crashes.
- Recommendation: return `None` for an empty source before calling MLIR, and report upstream.

#### MEL-52: More MLIR crashes from IR text

- Where: `melior/src/ir/attribute.rs:54-61`, `melior/src/ir/type.rs:57-64` and `melior/src/ir/module.rs:31-38`.
- Status: reproduced.

  | Input                                                                          | Observed                                            |
  | ------------------------------------------------------------------------------ | --------------------------------------------------- |
  | `#sparse_tensor.encoding<{}>`, also inside a tensor type or function signature | Segfault while parsing                              |
  | `#complex.number<:i32 1.0, 2.0>`                                               | Segfault while parsing                              |
  | Generic `"llvm.invoke"()[^bb1, ^bb2]` without a callee or operands             | Parses and verifies; printing dies with a bus error |

- Impact: a few dozen bytes of untrusted text crash a consumer that registered all dialects, and nothing in Melior can check first. A sub-audit sent 63,680 inputs through both parsers and every typed accessor and found no crash class beyond these, MEL-47 and MEL-51.
- Recommendation: report upstream with MEL-47 and MEL-48.

### Medium

#### MEL-12: Callback contracts are not enforced

- Where: `melior/src/context.rs:107-140`; `melior/src/rewrite_pattern.rs:28-32`, `:52-68`.
- Status: reproduced under Guard Malloc.
- Problem: three cases segfault. A diagnostic handler that detaches itself frees its own closure while it runs. A handler that emits a diagnostic re-enters through the same user data and aliases its state. A pattern that erases its root and returns `false` leaves the driver holding a freed operation for the next pattern; the documentation now forbids it, but nothing enforces it.
- Recommendation: guard re-entry with a flag or `RefCell`, defer the destruction of handlers, and track modification in the rewriter to report a match whenever the IR changed.

#### MEL-13: Unverified IR reaches passes, translation and the JIT

- Where: `melior/src/pass/manager.rs:73-83`; `melior/src/utility.rs:65-76`; `melior/src/execution_engine.rs:15-42`.
- Status: reproduced for passes; read for translation and the JIT.
- Problem: `OperationBuilder::build` doesn't verify, and the pass manager verifies only after each pass. 9 of 42 combinations of malformed operations and passes segfault, for example a `func.func` without attributes in `inline`, a `memref.load` without operands in `sccp`, and a `cf.br` without a successor in `canonicalize`. `verify()` itself is not a safe gate: the safe greedy driver segfaults on an `arith.addi` without operands even with an empty pattern set, and `ods::sparse_tensor::coiterate` with empty lists builds an operation whose `verify()` segfaults.
- Recommendation: verify in `PassManager::run`, `translate_module_to_llvm_ir` and `ExecutionEngine::new`, with an `unsafe` escape hatch.

#### MEL-14: Pass registration functions race on MLIR's global registry

- Where: `melior-macro/src/pass.rs:30-40`; `melior/src/utility.rs:28-33`.
- Status: reproduced. With eight threads registering different passes, a registration was lost in 25 of 110 runs, and one run crashed.
- Problem: all generated `register_*` functions are safe and insert into unsynchronized global maps. The `Once` in `register_all_passes` doesn't cover them, and parallel tests can hit this.
- Recommendation: one process-wide lock shared by all registration and pipeline parsing.

#### MEL-15: Bytes of printed text that aren't UTF-8 abort diagnostic handlers

- Where: `melior/src/utility.rs:91-107`; `melior/src/diagnostic.rs:68-82`; `melior/src/llvm/module.rs:48-60`.
- Status: reproduced. With `call @"\FF"()` in parsed source, `write!` returns an error after a partial write, and `diagnostic.to_string()` in a handler exits with status 134.
- Problem: MLIR prints raw symbol bytes in some diagnostics, and LLVM prints the target triple unescaped. `Display` then fails, `to_string()` panics, and a panic inside the `extern "C"` trampoline aborts.
- Impact: untrusted IR text aborts a process whose handler formats diagnostics, as the crate's own test helper does.
- Recommendation: convert lossily in the print callback and in the `Display` implementation of LLVM modules.

#### MEL-16: `MemRefType::strides_and_offset` ignores failure

- Where: `melior/src/ir/type/mem_ref.rs:95-107`.
- Status: reproduced by a sub-audit.
- Problem: layouts that are not strided return `([0, 0], 0)`, and dynamic values come back as an undocumented `i64::MIN` sentinel.
- Recommendation: return `Result`.

#### MEL-17: `IntegerAttribute` accessors truncate wide values

- Where: `melior/src/ir/attribute/integer.rs:24-36`.
- Status: reproduced by a sub-audit.
- Problem: `2^64 : i128` reads as 0 through all three accessors, and `255 : ui8` reads as −1 through `value()`.
- Recommendation: check the bit width, or expose the words.

#### MEL-18: The `*Like` traits are safe and unsealed

- Where: `melior/src/ir/type/type_like.rs:6-8`; `melior/src/ir/attribute/attribute_like.rs:12-14`; `melior/src/ir/type/shaped_type_like.rs:21`; the same shape in the traits of values, blocks, operations and regions.
- Status: reproduced by a sub-audit under `#![forbid(unsafe_code)]`.
- Problem: a downstream `to_raw` that returns a null handle or one of the wrong kind makes every provided method undefined.
- Recommendation: seal the traits, or make them `unsafe trait`.

#### MEL-19: Deeply nested input overflows the stack

- Where: `melior/src/ir/type.rs:57-64`; `melior/src/ir/attribute.rs:54-61`; `melior/src/ir/module.rs:31-38`; `melior/src/utility.rs:36-55`.
- Status: reproduced by a sub-audit with 100,000 nested `tuple<`, and with `parse_pass_pipeline` at 100,000 nesting levels.
- Problem: MLIR's parsers and printers recurse without a depth limit, and so does the greedy rewrite driver, once per nested region (KAL-17).
- Recommendation: document it and offer a depth check before parsing.

#### MEL-20: `cf::switch` doesn't validate list lengths or case values

- Where: `melior/src/dialect/cf.rs:102-167`.
- Status: reproduced by a sub-audit; the segfault was reproduced without any operand lists.
- Problem: fewer operand lists than destinations make `verify()` read out of bounds and segfault, and extra lists are accepted silently. Case values are narrowed to the width of the flag, so `[1, 257]` on `i8` gives two cases of `1`.
- Recommendation: return an error on unequal lengths and unrepresentable values, and take the type from the flag.

#### MEL-21: `cf::cond_br` derives segment sizes from block argument counts

- Where: `melior/src/dialect/cf.rs:71-79`.
- Status: read; the behavior reproduced by a sub-audit.
- Problem: a branch built before its successors get arguments is invalid, and a wrong split silently moves operands between the two sides and still verifies.
- Recommendation: use the lengths of the operand slices.

#### MEL-22: `llvm::call_intrinsic` never builds a valid operation

- Where: `melior/src/dialect/llvm.rs:241-254`.
- Status: read against `LLVMOps.td`; reproduced by a sub-audit.
- Problem: `op_bundle_sizes` and `operandSegmentSizes` are missing, and there is no test.
- Recommendation: set both, and take `Option<Type>` for the result.

#### MEL-23: `arith::addui_extended` panics on every call

- Where: `melior/src/dialect/arith.rs:150`; `melior-macro/src/operation.rs:15-18`, `:91-94`.
- Status: reproduced.
- Problem: the operation has no type inference, and the macro requests it unconditionally.
- Recommendation: build it with explicit result types, and add a smoke test over every constructor generated by the macro.

#### MEL-24: `llvm::attributes::linkage(Linkage::LinkOnce)` panics

- Where: `melior/src/dialect/llvm/attributes.rs:32`, `:38`.
- Status: read against `LLVMEnums.td`.
- Problem: the keyword is `linkonce`, not `link_once`, and only `External` is tested.
- Recommendation: fix the keyword and test every variant.

#### MEL-53: Dropping an `ExecutionEngine` runs code from the module

- Where: `melior/src/execution_engine.rs:15-47`, `:87-96`.
- Status: reproduced. The module's `llvm.mlir.global_dtors` hook calls `exit(42)`. After `lookup("main")` or `dump_to_object_file`, dropping the engine exits with status 42; without either call, the drop runs nothing.
- Impact: `invoke_packed` is `unsafe` because it runs compiled code, but these safe paths run it too, so a tool that only writes object files from untrusted IR executes that IR. A sub-audit rated it High; it is rated Medium here, because handing IR to a JIT usually implies trust.
- Recommendation: make `new` `unsafe` with this contract, or reject modules with global constructors and destructors in a safe constructor.

#### MEL-54: Nested external passes race on shared IR

- Where: `melior/src/pass/external.rs:70-80`, `:123-143`, `:177-218`.
- Status: reproduced. A pass nested under `func.func` appends one operation to the module body per function. With eight worker threads and 2,000 functions, three runs appended 788, 342 and 2,000 operations; single-threaded runs always appended 2,000.
- Problem: the callback gets a full operation reference that can reach the parent and the siblings that other threads are processing. The `Send` bound only covers the state of the pass.
- Recommendation: make adding an external pass to a nested manager `unsafe` with a contract to touch only the given operation, or pass a handle without upward navigation.

#### MEL-55: Safe calls that abort the process

- Where: `melior/src/pass/manager.rs:44-46`, `:57-70`; `melior/src/pass/operation_manager.rs:30-32`; `melior/src/utility.rs:59-61`.
- Status: reproduced; each exits with status 134 and an `LLVM ERROR`.
- Problem: `add_pass` on a nested manager with a pass restricted to another operation, `enable_ir_printing` with module scope while threading is enabled, which is the default, and `load_irdl_dialects` with a dialect named like a loaded one (`builtin`, `func`).
- Recommendation: check in Rust and return `Result`.

#### MEL-56: LLVM dialect element types are not validated, and verified modules crash translation

- Where: `melior/src/dialect/llvm/type.rs:15-33`, `:50-59`; `melior/src/dialect/llvm.rs:44-67`, `:163-176`; `melior/src/helpers/llvm.rs:231-254`, `:290-322`.
- Status: reproduced. `llvm.alloca %n x index` verifies and segfaults in `translate_module_to_llvm_ir`. `array(void, 2)` builds `!llvm.array<2 x void>`, which MLIR's own parser rejects, and an `alloca` of it verifies and aborts translation with status 133.
- Impact: verifying before translation, as MEL-13 recommends, doesn't catch it.
- Recommendation: use the checked getters and return `Result`, reject void, function and non-LLVM element types in Rust, and report the verifier gap upstream.

#### MEL-57: `verify()`, `Display` and `Debug` segfault on three terminators in a detached block

- Where: `melior/src/ir/operation/operation_like.rs:253-255`; `melior/src/ir/operation.rs:113-135`, `:219-229`.
- Status: reproduced for `ods::linalg::yield`, `ods::linalg::index` and `ods::affine::yield` appended to a block that is not attached to an operation yet. They are fine once attached, and `scf.yield` is fine.
- Problem: these verifiers dereference the parent operation, and printing runs the verifier. Melior builds region bodies bottom-up, so the block is always detached at that point.
- Recommendation: print in the generic form when the operation has no enclosing operation, and report upstream.

#### MEL-58: `IntegerAttribute::new` zero-extends negative values above 64 bits

- Where: `melior/src/ir/attribute/integer.rs:19-21`.
- Status: reproduced. `new(i128, -1)` gives `18446744073709551615 : i128`, which differs from the parsed `-1 : i128`, while `i64` and `si128` are correct. `value()` truncates (MEL-17) and returns −1, which hides it.
- Recommendation: build wide values from words with sign extension, or return an error.

### Low

| ID     | Finding                                                                                                                                                                                                                                                                  | Location                                                                                                                                                                                | Status                              |
| ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------- |
| MEL-25 | Leaks: a dropped `OperationBuilder` leaks its arrays and regions; `RewritePattern` and `Pass` have no destructor; `RewriterBase::insert` without an insertion point leaves the operation detached and unowned                                                            | `ir/operation/builder.rs:20-23`; `rewrite_pattern.rs:17-22`; `pass.rs:41-46`; `ir_rewriter.rs:128-130`                                                                                  | Read; `insert` reproduced           |
| MEL-26 | Panics where errors are expected: `Module::parse` on an interior NUL; `previous_in_block` is `todo!()`; `fused_location` indexes unchecked; constructors based on inference `expect` when the dialect is not loaded                                                      | `ir/module.rs:34`; `ir/operation/operation_like.rs:242-245`; `ir/location.rs:166-177`; `dialect/arith.rs:23`, `:128`; `dialect/index.rs:24-61`; `dialect/memref.rs:116`, `:269`, `:278` | NUL reproduced; read                |
| MEL-28 | Silent no-ops: `set_operands` on an operation without operands such as `arith.constant`; `replace_all_op_uses_with_values` replaces only a prefix on a count mismatch; `ExecutionEngine::new` accepts any optimization level; `dump_to_object_file` reports nothing      | `ir/operation/operation_like.rs:536-544`; `ir_rewriter.rs:325-338`; `execution_engine.rs:15-42`, `:87-89`                                                                               | Operands and level reproduced; read |
| MEL-29 | Integer conversions truncate or change sign: `u64` dimensions (`1 << 63` becomes dynamic), `usize` positions narrowed to 32 bits (`multi_dim_identity(&context, 1 << 32)` has no dimensions), `IntegerAttribute::new(i8, 300)` is 44, and lines and columns of locations | `ir/type/ranked_tensor.rs:19-46`; `ir/affine_expr.rs:39-46`; `ir/affine_map.rs`; `ir/location.rs:35-44`                                                                                 | Reproduced by a sub-audit           |
| MEL-30 | `TypeId::create` returns an unbounded lifetime, so a pass ID can come from a dead local; `Allocator::allocate_type_id(&mut self)` allows one live ID                                                                                                                     | `ir/type/id.rs:19-54`; `ir/type/id/allocator.rs:22-24`; `pass/external.rs:177-179`                                                                                                      | Reproduced by a sub-audit           |
| MEL-31 | Checked constructors can't express defaults, and parameter orders and dimension types differ between siblings                                                                                                                                                            | `ir/type/ranked_tensor.rs:31-46`; `ir/type/mem_ref.rs:38-55`                                                                                                                            | Reproduced by a sub-audit           |
| MEL-32 | Dense elements: a single value is accepted for a zero-element shape; `raw_data()` returns a pointer without a length; padding bits are not masked                                                                                                                        | `ir/attribute/dense_elements.rs:173-189`, `:273-283`, `:491-501`                                                                                                                        | Reproduced by a sub-audit           |
| MEL-33 | `is_multiple_of(0)` and `i64::MIN` operands reach undefined integer arithmetic in MLIR, which doesn't trap on Arm                                                                                                                                                        | `ir/affine_expr.rs:59-61`, `:103-105`, `:173-175`                                                                                                                                       | Read                                |
| MEL-34 | `memref::view` attaches a stray `operand_segment_sizes` attribute, and five constructors rely on the legacy alias                                                                                                                                                        | `dialect/memref.rs:68`, `:150-153`, `:180`; `dialect/cf.rs:71`, `:147`                                                                                                                  | Reproduced by a sub-audit           |
| MEL-35 | `llvm::alloca` with default options omits the required `elem_type`, and the documentation of `AllocaOptions` is stale                                                                                                                                                    | `dialect/llvm.rs:163-176`; `dialect/llvm/alloca_options.rs:22`, `:33`                                                                                                                   | Reproduced by a sub-audit           |
| MEL-36 | Helper traits: `gep` always returns address space 0; `GepIndex::Const(i32::MIN)` collides with the dynamic marker; `alloca` alignment is unchecked; `store` and `memcpy` reverse the parameter order of the dialect; `append_op_result` appends before checking          | `helpers/llvm.rs:95-111`, `:248-251`, `:303-313`; `helpers/builtin.rs:22-24`                                                                                                            | Reproduced by a sub-audit           |
| MEL-37 | Duplicate attribute keys: `add_attributes` appends, so extra attributes of `func::func` can override `sym_name` or repeat a key, and the printed form doesn't parse back                                                                                                 | `ir/operation/builder.rs:117-132`; `dialect/func.rs:67-72`; `dialect/llvm.rs:218-223`                                                                                                   | Reproduced by a sub-audit           |
| MEL-38 | `melior::dialect!` is exported regardless of features, but its output names a module that needs `ods-dialects`, and CI never builds the default features alone                                                                                                           | `lib.rs:48`; `dialect.rs:21-22`                                                                                                                                                         | Reproduced by a sub-audit           |
| MEL-59 | A pattern callback can re-enter itself by applying the frozen set that contains it; the inner call freed a `Vec` the outer one still borrowed. It needs a leaked `&'static Context`. Same class as MEL-12                                                                | `rewrite_pattern.rs:52-68`                                                                                                                                                              | Reproduced                          |
| MEL-60 | Unchecked constructors build values that pass `verify()` but that MLIR can't parse back: `vector<0xi32>`, `tensor<2xnone>`, `memref<-5xf32>`, and a `FloatAttribute` of type `index`. Extends MEL-07 and MEL-08                                                          | `ir/type.rs:157-165`, `:184-205`; `ir/type/ranked_tensor.rs:19-28`; `ir/type/mem_ref.rs:20-35`, `:58-71`; `ir/attribute/float.rs:19-27`                                                 | Reproduced by a sub-audit           |
| MEL-61 | `DictionaryAttribute::new` accepts duplicate keys, and the printed form doesn't parse back. Same defect as MEL-37 through another API                                                                                                                                    | `ir/attribute/dictionary.rs:16-28`                                                                                                                                                      | Reproduced by a sub-audit           |
| MEL-62 | Dense elements with a dialect element type such as `!tosa.mxint8` can only be parsed; `new`, `splat` and `raw_buffer` always fail for them                                                                                                                               | `ir/attribute/dense_elements.rs:173-189`, `:474-489`, `:536-555`                                                                                                                        | Reproduced by a sub-audit           |
| MEL-63 | Silent no-ops that extend MEL-28: driver limits of 0 or below −1 return `Ok` without calling any pattern; `ExecutionEngine::new` ignores a library that fails to load; functions reached through `lookup` run without global constructors                                | `greedy_rewrite_driver.rs:108-120`; `execution_engine.rs:22-47`                                                                                                                         | Limits reproduced; partly read      |
| MEL-64 | `arith::constant` panics for any attribute without a type when the dialect is loaded. Extends MEL-26                                                                                                                                                                     | `dialect/arith.rs:14-24`                                                                                                                                                                | Reproduced by a sub-audit           |
| MEL-65 | `memref::global` without a value emits an uninitialized definition, and an external declaration can't be expressed                                                                                                                                                       | `dialect/memref.rs:230-233`                                                                                                                                                             | Reproduced by a sub-audit           |

Locations in this table are relative to `melior/src/`.

### Informational

| ID     | Finding                                                                                                                                                                                                                                                                 | Location                                                                                                                                                  |
| ------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| MEL-40 | The README's safety section cites `Region::append_block`, which owner lifetimes already prevent, and omits the classes above other than the parser defects documented by #1044                                                                                          | `README.md:101-118`                                                                                                                                       |
| MEL-41 | Reference types are transmuted to owned types without `#[repr(transparent)]`, `add_regions_vec` transmutes a `Vec`, and about 600 `unsafe` blocks have no safety comment                                                                                                | `ir/block.rs:175-181`; `ir/region.rs:107-113`; `ir/operation.rs:159-164`, `:275-281`; `context.rs:216-221`; `ir/operation/builder.rs:83-96`; `lib.rs:2-3` |
| MEL-42 | `DistinctAttribute` can't be named outside the crate                                                                                                                                                                                                                    | `ir/attribute.rs:20-33`                                                                                                                                   |
| MEL-43 | A panic in the caller's `fmt::Write` aborts inside the print callback, and error paths format the whole value                                                                                                                                                           | `utility.rs:91-107`                                                                                                                                       |
| MEL-44 | Documentation drift: the range of `shift_dims`, `is_pure_affine`, "only valid if" on safe functions, the feature comment in `Cargo.toml`, `extsi` and `trunci` documented as `arith.extui`, and an incomplete safety section of `invoke_packed`                         | `ir/affine_expr.rs:156-200`; `ir/type.rs:141`; `Cargo.toml:24-25`                                                                                         |
| MEL-45 | Test gaps: no tests of wrong kinds or boundaries for the findings above; `dump_to_object_file` writes to a fixed `/tmp/melior/test.o` and asserts nothing; the `enable_ir_printing` test is commented out; `convert_module` never lowers `arith`; helpers have no tests | `execution_engine.rs:147-173`; `pass/manager.rs:172-178`; `dialect/llvm.rs:394-410`                                                                       |
| MEL-46 | Coverage: 21 `mlirCreate*` passes are not wrapped, 16 registered dialects have no generated module, `ods::transform` has 37 of 234 operations, and there are 15 dialect handles                                                                                         | `pass/*.rs`; `dialect/ods.rs`; `dialect/handle.rs`                                                                                                        |

Locations in this table are relative to `melior/src/`, except for the README and `Cargo.toml`.

### Reviewed without findings

- Context, owner and callback lifetimes: all 30 compile-fail tests fail for the reason their names claim.
- Index bounds of every accessor, the `TryFrom` predicates and accessors of every typed wrapper, and slices cast to C arrays, which are all transparent.
- Ownership transfer in `freeze`, `add_pass`, the `append_*` methods and the user data of external passes. `freeze` followed by the destructor of the set is not a double free under Guard Malloc.
- The rewrite pattern modules after they were split and bound to their contexts, apart from MEL-59.
- Auto traits of all 36 handle types (none is `Send` or `Sync`) and diagnostic handlers under threading.
- 128 cases of hand-written constructors with valid arguments, and 60,568 runs with wrong ones, which found no crash beyond `memref::load`. Names, attributes and operand orders were checked against the TableGen definitions of LLVM 23.1.6, and all 271 pass constructors link to the right C functions.
- Dialect handles and registries.

## `melior-macro`

### High

#### MAC-01: Generated safe builders run type inference on unverified operands

- Where: `melior-macro/src/dialect/operation.rs:93-106`; `melior-macro/src/dialect/generation/operation_builder.rs:277-303`; `melior-macro/src/operation.rs:17`, `:33`.
- Status: reproduced. `ods::vector::to_elements(&context, i32_value, location)` segfaults, and `ods::sparse_tensor::positions` aborts with `SmallVector unable to grow`.
- Problem: for operations with an explicit `InferTypeOpInterface` and no regions, `build()` enables inference, so a `Value` of the wrong type reaches an unchecked `cast<>`. 36 bundled operations take this path. In a sweep with `i32` operands, 15 of them segfault or abort, 6 panic and 15 complete, and `tosa::reshape` returns a tensor of 32 dimensions. It also reaches well-typed inputs: `ods::tosa::reduce_sum` on `tensor<2x3xf32>` with axis −1 infers `tensor<2xf32>`, and axis −2 aborts.
- Recommendation: take explicit result types, or mark the inferring constructors `unsafe`. It is the same decision as MEL-09.

#### MAC-16: Removing or mistyping a default-valued attribute leaves a verified operation that segfaults

- Where: `melior-macro/src/dialect/operation/attribute.rs:114-116`; `melior-macro/src/dialect/generation/attribute_accessor.rs:33-62`; `melior-macro/src/dialect/generation/operation_builder.rs:183-199`.
- Status: reproduced.
  - `ods::arith::addi(..)` followed by `remove_overflow_flags()`: `verify()` returns true, and `Display` segfaults.
  - `ods::llvm::load(..)` followed by `remove_ordering()`: `verify()` segfaults.
  - `set_fastmath` with a string attribute, or the builder's `fastmath(Some(..))` with one, leaves the same state with the same crash.
- Problem: MLIR stores these attributes as properties filled with their defaults, and the generated C++ getters and printers don't expect null. `mlir-tblgen` emits removers only for optional attributes, but the macro treats default-valued ones as optional too. A value of the wrong kind has the same effect, because MLIR drops it.
- Reach: 221 such attributes on 196 bundled operations. In a sweep of 170 of them by a sub-audit, 93 crashed in `Display` after `verify()` returned true and 5 crashed inside `verify()`, including every `arith` and `complex` attribute exercised.
- Recommendation: generate removers only for optional attributes, and after a setter of an untyped attribute, read it back and fail if MLIR dropped it.

### Medium

#### MAC-02: Valid ODS names make the macro panic or emit invalid identifiers

- Where: `melior-macro/src/dialect/utility.rs:39`, `:72`; `melior-macro/src/dialect/generation.rs:27`; `melior-macro/src/dialect/operation.rs:159`.
- Status: reproduced by a sub-audit.
- Problem: `$self`, `$super` and `$crate`, a record named `X_3DOp`, and a summary that isn't ASCII each make the procedural macro panic, and `gen` is emitted unescaped in an edition 2024 crate. No bundled operation is affected.
- Recommendation: rename keywords that can't be raw identifiers, validate identifiers with `syn::parse_str`, and slice by characters.

#### MAC-03: Generated names collide with ODS names

- Where: `melior-macro/src/dialect/generation/operation_builder.rs:171`, `:190`, `:238`, `:263`, `:360`, `:389`; `melior-macro/src/dialect/operation/attribute.rs:28-29`.
- Status: reproduced by a sub-audit.
- Problem: fields named `context`, `location`, `new`, `build` or `result_type`, a field `set_x` next to an attribute `x`, and records `X_Foo` next to `X_FooOp` produce compiler errors spanned at the `dialect!` call.
- Recommendation: use `Span::mixed_site()` for generated locals, a list of reserved names, and a check for duplicates up front.

#### MAC-04: `SameVariadic*Size` with an optional element doesn't compile

- Where: `melior-macro/src/dialect/generation/element_accessor.rs:56-64`.
- Status: reproduced by a sub-audit (E0308).
- Recommendation: return the single element or an error, as the branch for attribute-sized segments does.

#### MAC-17: Values of the wrong kind for attributes typed as plain `Attribute` are discarded silently

- Where: `melior-macro/src/dialect/operation/attribute.rs:68-84`, `:118-128`; `melior-macro/src/dialect/generation/attribute_accessor.rs:39-44`.
- Status: reproduced. `LoadOperation::builder(..).volatile(Some(..))` with a boolean attribute verifies and prints without the flag.
- Reach: 610 of 1,268 attribute parameters are untyped.
- Recommendation: type the parameters from their ODS constraints, or check what MLIR stored after setting them.

### Low

| ID     | Finding                                                                                                                                                                                  | Location                                                              | Status                    |
| ------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------- | ------------------------- |
| MAC-05 | Unnamed regions and successors reject the whole dialect, although `mlir-tblgen` accepts them                                                                                             | `dialect/operation.rs:279-300`                                        | Reproduced by a sub-audit |
| MAC-06 | Derived attributes are named after the anonymous record and read from the dictionary (`anonymous_365()`). It affects custom ODS only, since no installed dialect has a derived attribute | `dialect/operation.rs:480-499`                                        | Reproduced by a sub-audit |
| MAC-07 | Accessors overflow on operations that don't match the ODS layout, because `TryFrom` checks only the name                                                                                 | `dialect/generation/element_accessor.rs:34`, `:41`, `:67`, `:100-103` | Reproduced by a sub-audit |
| MAC-08 | Constructors panic on an empty variadic first operand and on inference failure. It affects exactly `irdl.all_of`, `irdl.any_of` and `transform.merge_handles`                            | `dialect/generation/operation_builder.rs:229-230`, `:300-303`         | Reproduced by a sub-audit |
| MAC-09 | Setters of optional attributes can be called twice, giving duplicate dictionary keys                                                                                                     | `dialect/generation/operation_builder.rs:183-199`                     | Reproduced                |
| MAC-10 | The build script runs `llvm-config` through a shell with an unquoted prefix, ignores failure, which leaves an empty include directory, and doesn't rerun when LLVM changes               | `build.rs:11`, `:25-43`                                               | Reproduced                |
| MAC-11 | `.td` files and environment variables read by `dialect!` don't trigger recompilation                                                                                                     | `dialect.rs:29-47`                                                    | Reproduced by a sub-audit |
| MAC-12 | `dialect!` without `files` segfaults the compiler, and a `.td` file without the classes `Dialect` or `Op` makes it exit                                                                  | `dialect.rs:41-47`, `:52`, `:69-70`                                   | Reproduced by a sub-audit |
| MAC-18 | Properties that aren't attributes are dropped, so seven bundled operations lose flags: `llvm.add`, `sub`, `mul`, `shl`, `trunc`, `llvm.getelementptr` and `affine.linearize_index`       | `dialect/operation.rs:391-408`, `:460-478`                            | Reproduced by a sub-audit |
| MAC-19 | On dialects not registered in MLIR, default-valued attributes have no default, so the accessor returns an error, and `set_flag(false)` on a unit attribute leaves it present             | `dialect/generation/attribute_accessor.rs:17-31`                      | Reproduced by a sub-audit |

Locations in this table are relative to `melior-macro/src/`, except for `build.rs`.

### Informational

| ID     | Finding                                                                                                                                                                                                                      | Location                                                                                 |
| ------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- |
| MAC-13 | `files` paths are spliced unescaped into TableGen source, and the working directory of the compiler shadows include directories                                                                                              | `dialect.rs:42-44`, `:105-116`                                                           |
| MAC-14 | Two UI tests reach their `.td` files relative to the target directory and fail under `CARGO_TARGET_DIR`, and there are no tests for the operation enum, successor accessors, removers, or default-valued and unit attributes | `tests/ui/unnamed_result_conflicting_with_*.rs:4`                                        |
| MAC-15 | Builders accept unequal `SameVariadic*Size` groups, and accessors then return wrong ranges                                                                                                                                   | `dialect/generation/operation_builder.rs:203-213`                                        |
| MAC-20 | The macro accepts ODS that `mlir-tblgen` rejects: one variadic group with a segment trait, both size traits together, unnamed attributes, and a variadic region that is not last                                             | `dialect/operation.rs:417-429`, `:463-466`; `dialect/generation/region_accessor.rs:9-13` |

### Reviewed without findings

- Index and length computation for all five layouts on well-formed operations, the ordering of type states, which can't misplace operands in any allowed order, the selection of `SameOperandsAndResultType` and `FirstAttrDerivedResultType`, the mapping of attribute storage types, successor entry points, which are all `unsafe fn`, and the macros other than `dialect!`.
- The operand, result, region, successor and attribute layouts of all 1,257 bundled operations match the installed generated C++, and builders round-trip through accessors in 3,771 runs with 18,265 checks.

## `melior-kaleidoscope`

The Toy compiler (`publish = false`) miscompiled no valid program: a sub-audit compared about 10,000 generated programs against an independent reference interpreter, optimized and unoptimized, without a mismatch. Two fuzzers with 5,000 mutated and 3,000 random programs produced only exit statuses 0 and 1.

### High

#### KAL-01: Crafted `.mlir` input crashes the compiler with use-after-free

- Where: `melior-kaleidoscope/src/main.rs:62-72`; `melior-kaleidoscope/src/canonicalize.rs:139-254`, `:276-308`; `melior-kaleidoscope/src/lower_to_affine.rs:182`, `:336`, `:468-469`; `melior-kaleidoscope/src/shape_inference.rs:28-35`.
- Status: reproduced with three inputs, which still segfaulted at `26c630dd7`.
- Problem: the passes assume the shape of the output of code generation: one block, no nested regions, one result, and block arguments that match the function type. `.mlir` input is parsed and compiled without a structural check, so a use inside a nested region, a `toy.transpose` with two results, or a block argument leads to erasing IR that is still used. Results of the wrong type print heap garbage under the JIT.
- Impact: tempered by `publish = false`, and running an untrusted `.mlir` file under the JIT executes arbitrary code by design.
- Recommendation: validate every module that doesn't come from code generation in one pass.

### Medium

#### KAL-02: Unbounded recursion aborts on small sources

- Where: `melior-kaleidoscope/src/parse.rs:205-317`; `melior-kaleidoscope/src/generate/function.rs:152-217`; `melior-kaleidoscope/src/ast/binary.rs:8-9`.
- Status: reproduced. 5,000 nested parentheses overflow the stack, with exit status 134.
- Problem: nesting of `(`, `[`, `{` and `transpose(` recurses in the parser, and a flat `+` chain recurses in code generation and in the destructor of the syntax tree (50,000 terms in a release build).
- Recommendation: a depth limit in the parser, and iterative handling of left-deep chains.

#### KAL-03: Broadcast constants are expanded element by element

- Where: `melior-kaleidoscope/src/lower_to_affine.rs:115-180`; `melior-kaleidoscope/src/canonicalize.rs:180-189`.
- Status: reproduced by a sub-audit.
- Problem: `var a<1152921504606846975> = 1; print(a);` hangs with growing memory in affine lowering, and `var a<300000> = 1` emits 300,000 stores.
- Recommendation: lower splats to a loop nest and cap element-wise expansion.

#### KAL-04: Inlining is exponential on doubling call chains

- Where: `melior-kaleidoscope/src/inliner.rs:32-38`, `:102-106`.
- Status: reproduced by a sub-audit: 22 levels in 850 bytes took 13 seconds and 3.4 GB.
- Recommendation: canonicalize callees before cloning them, inline only what `main` reaches, and enforce a budget.

#### KAL-05: Reshape folding accepts mismatched element counts and fabricates data

- Where: `melior-kaleidoscope/src/canonicalize.rs:168-189`.
- Status: reproduced.
- Problem: `var a<2, 3> = [1, 1]` prints six ones, and `var a<0> = 1; var b<2> = a;` prints two, while `var a<2, 3> = [1, 2]` is rejected. Whether a program is accepted depends on whether the elements happen to be equal.
- Recommendation: broadcast only from rank 0, and otherwise require equal counts.

#### KAL-17: Tensor rank is unbounded, so a flat shape list overflows the stack inside MLIR

- Where: `melior-kaleidoscope/src/parse.rs:196-203`; `melior-kaleidoscope/src/generate/module.rs:183-203`.
- Status: reproduced. A 24 KB program that declares a tensor of rank 8,000 and prints it exits with status 134, and with a transpose and `--emit mlir-affine`, rank 2,000 in 6 KB is enough.
- Problem: the shape check bounds the size in bytes, not the rank. Each dimension becomes a nested loop region, and MLIR's region simplification and printer recurse once per level, so a depth limit in the parser (KAL-02) doesn't close it.
- Recommendation: reject ranks above a small bound, and bound the nesting depth of structs as well.

### Low

| ID     | Finding                                                                                                                                                                                                                                  | Location                                                                         | Status                    |
| ------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------- | ------------------------- |
| KAL-06 | A zero-sized dimension bypasses the size bound: `var a<1152921504606846975, 0> = 1; print(a);` printed 279 MB of newlines in 10 seconds                                                                                                  | `generate/module.rs:188-196`; `lower_to_affine.rs:286-316`                       | Reproduced                |
| KAL-07 | `inline_functions` takes `&Module` but erases functions, so handles held by the caller dangle                                                                                                                                            | `inliner.rs:18`, `:41-47`, `:159-160`                                            | Reproduced by a sub-audit |
| KAL-08 | `run_jit` is safe but assumes that `main` takes and returns nothing; a `main` in a `.mlir` file with a memref parameter segfaults                                                                                                        | `compile.rs:88-95`; `lower_to_llvm.rs:75-90`                                     | Reproduced by a sub-audit |
| KAL-09 | Lowering patterns insert operations and then return `false`, which leaves stray `memref.alloc` pairs after a failed constant                                                                                                             | `lower_to_affine.rs:126-137`, `:281-323`, `:453-466`; `lower_to_llvm.rs:120-182` | Reproduced by a sub-audit |
| KAL-10 | Missing semantic checks: a user function named `transpose` is silently shadowed; wrong arity passes `--emit mlir`; duplicate struct members are accepted; functions without a return value can't be called; dead ill-typed code compiles | `generate/function.rs:115-147`, `:296-365`; `generate/module.rs:69-106`          | Reproduced                |
| KAL-11 | Late errors carry no source location or name the wrong cause, such as shape mismatches, a missing `main` and element count mismatches                                                                                                    | `shape_inference.rs:45-100`; `inliner.rs:109-115`; `compile.rs:92`               | Reproduced by a sub-audit |
| KAL-12 | A comment that contains a lone carriage return is a syntax error                                                                                                                                                                         | `parse.rs:401-410`                                                               | Reproduced by a sub-audit |
| KAL-13 | A NUL byte in a `.mlir` input panics, with MEL-26 as its root cause                                                                                                                                                                      | `main.rs:67`                                                                     | Reproduced by a sub-audit |
| KAL-18 | A tensor passed for a struct parameter compiles and runs: `def f(s Foo) { return s; }` called with `f([1, 2])` prints `1 2`. Extends KAL-10                                                                                              | `generate/function.rs:338-365`; `inliner.rs:130-142`; `shape_inference.rs:77`    | Reproduced                |
| KAL-19 | A reshape of a reshape hides an invalid inner reshape: `var a<5> = [1, 2, 3, 4, 5, 6]; var b<2, 3> = a;` is accepted, while printing `a` directly is rejected. Extends KAL-05                                                            | `canonicalize.rs:168-174`                                                        | Reproduced                |

Locations in this table are relative to `melior-kaleidoscope/src/`.

### Informational

| ID     | Finding                                                                                                                                                                                                                                       | Location                                                      |
| ------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- |
| KAL-14 | Other super-linear behavior: nested struct types print exponentially (766 bytes of source gave 5.9 GB of output), `--emit ast` is cubic in the length of a chain, `--optimize` scales steeply with rank, and the inliner scans quadratically  | `generate/module.rs:77-103`; `main.rs:51`; `inliner.rs:32-74` |
| KAL-15 | Test gaps: no reshape of a non-scalar splat, nesting limits, malformed MLIR structure, or comparison of optimized and unoptimized output, and `examples/ast.toy` promises an error that never occurs                                          | `tests/command.rs`; `examples/ast.toy:24-26`                  |
| KAL-16 | The README doesn't state the deviations from the tutorial language: parameters written as `name Type`, `print` only as a statement, rejected reshapes of computed values, no numbers with leading dots, and shape mismatches of `+` as errors | `README.md`                                                   |
| KAL-20 | Parser diagnostics name the wrong cause, such as ``expected `>` `` for a dimension out of range                                                                                                                                               | `parse.rs:196-203`, `:301-332`                                |

Locations in this table are relative to `melior-kaleidoscope/`, and to its `src/` for source files.

### Reviewed without findings

The lexer and number parsing, scoping and struct handling, shape inference, the canonicalization rewrites, affine lowering, including loop bounds and the transpose mapping up to rank 6, LLVM lowering, the eight uses of `ContextRef::to_ref`, each of which binds the context reference to a local that outlives the reference, and the exit statuses of the command.

## Repository

### Medium

#### INF-01: Dependency updates merge with no human review

- Where: `.github/workflows/dependabot.yaml:1-13`; `.github/dependabot.yml`.
- Status: verified with the GitHub API.
- Problem: the workflow enables auto-merge on every Dependabot pull request, including cargo, action and toolchain updates and major versions, and the ruleset of `main` requires only status checks. #1035, a major update of `comrak` in the procedural macro crate, and #1045 to #1047, which updated `clap`, `comrak` and `regex` for 0.30.0, were merged by `github-actions` without reviews.
- Impact: a compromised upstream release lands on `main` unreviewed, and the dependencies of `melior-macro` run at compile time in every downstream build.
- Recommendation: auto-merge only patch and minor updates of dependencies that don't run at build time, or require one approval, and check the author of the pull request instead of the actor of the event.

### Low

#### INF-02: The release job runs unpinned tools while it can mint publishing credentials

- Where: `.github/workflows/release.yaml:11-25`.
- Status: read; the environment policy was verified with the GitHub API.
- Problem: `cargo install cargo-workspaces` has no version and no `--locked`, and it runs in a job with job-level `id-token: write`, also on pull request runs. The `release` environment is restricted to protected branches, which limits pull request runs only if the trusted publisher on crates.io requires that environment, which couldn't be checked.
- Recommendation: pin the tool with `--locked` or use `cargo publish --workspace`, and split publishing into a job that runs only on `main` and alone holds `id-token: write`.

#### INF-03: Sanitizer and feature coverage

- Where: `.github/workflows/test.yaml:30-46`.
- Status: read.
- Problem: the `sanitize` job omits `--all-features`, so `helpers` never runs under AddressSanitizer, which only instruments Rust, so dangling handles inside MLIR go unseen. No job builds `melior` with only its default features (MEL-38).

### Informational

#### INF-04: Smaller items

- `bench` runs `cargo bench` on a workspace without benchmarks (`.github/workflows/test.yaml:47-52`).
- `dialect_build` ends in `diff … || true` and installs an unpinned `cargo-expand` (`.github/workflows/test.yaml:53-64`).
- `document.yaml` grants `pages: write` and `id-token: write` on pull request runs (`.github/workflows/document.yaml:7-10`).
- `tools/api/difference.sh:7` uses a fixed `/tmp/melior`, and `tools/version.sh:11-15` splits its options into words.
- `proc-macro2 = "1"` and `quote = "1"` are loose requirements (`melior-macro/Cargo.toml:19-20`).

#### INF-05: The release dry run passes even when a crate fails to package

- Where: `.github/workflows/release.yaml:23`.
- Problem: `cargo workspaces publish --dry-run` turns failures of individual crates into warnings. On the pull request that bumped the version to 0.30.0 (#1048), it never packaged `melior`, because it couldn't resolve `melior-macro` 0.30.0 on crates.io, and the check still passed. The real release worked, as `cargo publish --workspace --dry-run` predicted locally.
- Recommendation: use `cargo publish --workspace --dry-run` in pull requests.

#### INF-06: The documentation site is stale

- Where: `.github/workflows/document.yaml`.
- Problem: `actions/deploy-pages` failed with "No artifacts named github-pages" on the pushes of #1044 and #1048, and on 3 of the last 8 pushes to `main` in all, without changes to the workflow. The `documentation` links of both crates point to the site, which still serves documentation without the safety warnings of #1044.
- Recommendation: rerun the deployment, and retry it on failure.

## Not covered

- Behavior on x86-64 and on Linux.
- LLVM built with assertions or sanitizers.
- Fuzzing of `Module::parse`. Only the type and attribute parsers were fuzzed.
- Whether types with element counts that overflow, built without parsing, for example by `MemRefType::new`, reach MLIR passes or lowering.
- The unmerged remote branch `bug/rewriter-erasure`.

## Recommendations

1. Fix MEL-51 and MAC-16. Both are small changes that close crashes in ordinary use.
2. Fix MEL-49 by loading the dialect of a name when it is first used, and MEL-50 with exclusive borrows of the pass manager.
3. Decide whether inference (MEL-09 and MAC-01) and the erasing rewriter methods (MEL-03) become `unsafe`. These two decisions close the most reachable crashes.
4. Fix MEL-04, MEL-05 and MEL-06 with owner lifetimes for operands and structural checks, which need a design decision.
5. Fix MEL-07, MEL-08, MEL-10 and MEL-11 with mechanical predicate and null checks that return `Result`.
6. Report MEL-47 (with the prepared fix), MEL-48, MEL-52 and MEL-57 upstream, along with the verifier gaps behind MEL-56 and MAC-16.
7. Fix INF-01, INF-02 and INF-05, and the constructors that always fail (MEL-22, MEL-23 and MEL-24) with a smoke test over every constructor.
