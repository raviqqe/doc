---
description: An AI audit of what remains before the ODS-generated dialects of Melior can replace its hand-written dialect modules, with measurements of the generated API, runtime type inference support, attribute typing, and build cost.
---

# Making the ODS-generated dialects the default in Melior

- Date: 2026-11-08
- Model: Claude Opus 5.5
- Repository: [`mlir-rs/melior`](https://github.com/mlir-rs/melior)
- Version: `bug/unnamed-operand` at `917be0139`, one commit on top of `main` at `e6407a664`

## Summary

- The goal is to retire the hand-written modules `melior::dialect::{arith, cf, func, index, llvm, memref, scf}` and make the generated `melior::dialect::ods::*` the default API.
- Coverage is not the problem. All seven hand-written dialects are among the bundled generated ones, and the `ods-dialects` feature adds about 7 seconds to `cargo check` and 11 seconds to `cargo build` of the crate.
- Four blockers remain: result type inference (179 builders demand result types that MLIR can infer), typed attributes (half of all attribute parameters are an untyped `Attribute`), the non-op items in the `llvm` module, and a migration path for functions that share names but not signatures.
- The main ergonomic gaps are that typed operations are lost once appended to a block, optional fields cannot be set conditionally, variadic-of-variadic operands are flattened, properties are dropped, and fields named `name` become `_name`.
- Five generator defects are still open, and none of them are needed to switch but all of them affect a default API.

## Method

- Generated API: the bundled dialects were expanded with `cargo expand --all-features dialect::ods`, and the signatures of every generated constructor and builder setter were extracted (1,257 operations).
- Result inference: a runtime probe asked MLIR, through `mlirOperationImplementsInterfaceStatic` with `mlirInferTypeOpInterfaceTypeID`, whether each operation whose builder demands result types implements `InferTypeOpInterface`. All 804 such operations were registered.
- Attributes and arguments: the bundled ODS files were dumped with `llvm-tblgen -dump-json`, and the attribute parameters, properties, and variadic-of-variadic operands of 1,191 operations were classified (a few bundled files were not dumped).
- Build cost: `cargo check -p melior` and `cargo build -p melior` were timed with and without `ods-dialects`, with dependencies already built and `CARGO_INCREMENTAL=0`.
- Code reading: all seven hand-written modules, the `helpers` traits, the README, and the generator in `macro/src/dialect`.

## Blockers

### B-1. 179 builders demand result types that MLIR can infer

- 804 of the 1,257 generated builders require result types, and only 36 use interface-based inference. The runtime probe found that 179 of those 804 operations implement `InferTypeOpInterface`.
- These include exactly the operations whose hand-written helpers infer types: `arith.cmpi`, `arith.cmpf`, `arith.constant`, and `arith.select`; `memref.load`, `memref.rank`, and `memref.dim`; all 24 `index` operations; and `scf.if`. For example, the generated `arith::constant(context, result, value, location)` needs a type the hand-written `arith::constant(context, value, location)` infers, and `index::add(context, result, lhs, rhs, location)` asks for a result that is always `index`.
- By dialect, the 179 operations are `index` 24, `amdgpu` 24, `gpu` 19, `llvm` 17, `shape` 13, `sparse_tensor` 10, `x86` 9, `async` 9, `vector` 8, `complex` 7, `memref` 6, `irdl` 6, `arm_sme` 6, `arith` 6, `tensor` 5, `math` 5, `affine` 3, `scf` 1, and `linalg` 1.
- Cause: the macro uses inference only when the TableGen trait list names `InferTypeOpInterface`, and never for operations with regions (lines 86 to 87 of `macro/src/dialect/operation.rs`). MLIR's own generator also adds the interface whenever every result type is derivable, from buildable types such as `Index` and `I1` or from relations such as `AllTypesMatch` and `TypesMatchWith`.
- Fix options: mirror that rule in the macro, or make result setters optional and call `enable_result_type_inference()` in `build()` when none were set. Either way, lift the region exclusion.

### B-2. Half of all attribute parameters are an untyped `Attribute`

- 607 of 1,218 attribute parameters fall through the storage type mapping in `build_storage_type` (lines 68 to 82 of `macro/src/dialect/operation/attribute.rs`) and become an untyped `Attribute`. The largest groups are below, and dialect enum attributes make up much of the rest.

  | Storage type                       | Parameters |
  | ---------------------------------- | ---------: |
  | `::mlir::UnitAttr`                 |        149 |
  | `::mlir::DenseIntElementsAttr`     |         97 |
  | `::mlir::arith::FastMathFlagsAttr` |         84 |
  | `::mlir::LLVM::FastmathFlagsAttr`  |         55 |
  | `::mlir::AffineMapAttr`            |         21 |
  | `::mlir::SymbolRefAttr`            |          8 |

- Parity is lost here: the hand-written modules provide `arith::CmpiPredicate`, `arith::CmpfPredicate`, and `llvm::attributes::Linkage`, while the generated `arith::cmpi(..., predicate: Attribute, ...)` and `index::cmp(..., pred: Attribute, ...)` give callers no safe way to build those attributes.
- Melior has no wrappers for unit, integer-element, affine-map, or symbol-reference attributes, or for any dialect enum attribute.
- Fix: add core wrappers for those attributes (a unit attribute is better as a `bool` parameter) and generate Rust enums from TableGen's enum attribute records.

### B-3. The non-op parts of `llvm` have nowhere to go

- The `llvm` module also holds the type constructors `pointer`, `struct`, `array`, `function`, and `void` (`melior/src/dialect/llvm/type.rs`), the `Linkage` attribute builder (`attributes.rs`), and the option builders `AllocaOptions` and `LoadStoreOptions` behind `llvm::alloca`, `llvm::load`, and `llvm::store`.
- It also has semantic helpers that a generated builder does not offer, such as `get_element_ptr_dynamic`, which writes the dynamic-index marker `i32::MIN` into `rawConstantIndices`.
- The macro reads only `Op` records and generates no types or attributes, so retiring the module needs either a permanent hand-written home for these items or type and attribute generation from TableGen's type and attribute definitions.

### B-4. Same names with different signatures leave no migration path

- The hand-written `arith::addi(lhs, rhs, location) -> Operation` and the generated `ods::arith::addi(context, lhs, rhs, location) -> AddIOperation` share every function name, so the two cannot live in one module.
- Generated constructors also take a `Context`, order parameters differently (results first, then operands, regions, successors, and attributes), and sometimes use different names, for example `extract_value` against `extractvalue` and `undef` against `mlir_undef` in `llvm`.
- Users of the hand-written functions include the README example, about 49 internal call sites outside the dialect modules, and the `helpers` traits, which already mix both sides (`ArithBlockExt` uses the hand-written `arith` together with `ods::arith::constant`, and `LlvmBlockExt` uses `ods::llvm` operations).
- A deprecation plan is needed, for example `#[deprecated]` hand-written functions pointing at their generated replacements for one release before `ods-dialects` becomes a default feature.

## High-impact ergonomics

### E-1. A typed operation is lost once it is in a block

- Generated operation structs convert only from an owned `Operation` (`TryFrom<Operation>` around line 89 of `macro/src/dialect/generation.rs`). `BlockLike::append_operation` takes `Operation` (line 90 of `melior/src/ir/block/block_like.rs`), so every call needs `.into()`, and the typed accessors are gone afterwards because there is no typed view over `OperationRef`.
- The typed structs do not implement `OperationLike` either. They expose only `as_operation()`.
- Recommended: let `append_operation` and the `insert_*` methods accept `impl Into<Operation>`, implement `OperationLike` for the typed structs, and add typed reference wrappers with `TryFrom<OperationRef>`.

### E-2. Optional fields cannot be set conditionally

- Optional operand and result setters change the builder type. For example, `.arg(value)` turns `llvm::ReturnOperationBuilder<'c, Unset>` into `ReturnOperationBuilder<'c, Set>`, so the two branches of an `if let Some(value)` around the setter have different types and callers must call `build()` in each branch.
- The hand-written functions take `Option` directly: `llvm::r#return(Option<Value>)`, `memref::realloc(size: Option<Value>)`, and `memref::alloc(alignment: Option<IntegerAttribute>)`.
- Setters that take `Option<T>` would remove the friction.

### E-3. Variadic-of-variadic operands are flattened

- Seven operations have them: `cf.switch`, `llvm.call`, `llvm.call_intrinsic`, `llvm.invoke`, `llvm.switch`, `llvm.indirectbr`, and `llvm.intr.assume`. Support is still a `TODO` at line 27 of `macro/src/dialect/type.rs`.
- The generated `cf::switch` takes one flat `case_operands` slice plus a hand-computed `case_operand_segments` attribute, while the hand-written `cf::switch` takes `&[&[Value]]` and computes the segments itself.

### E-4. Properties are silently dropped

- Operand and attribute collection keep only type constraints and attributes (lines 394 and 458 of `macro/src/dialect/operation.rs`), so property arguments disappear.
- Seven operations are affected: the overflow flags of `llvm.add`, `llvm.sub`, `llvm.mul`, `llvm.shl`, and `llvm.trunc`, the `noWrapFlags` of `llvm.getelementptr`, and the `disjoint` flag of `affine.linearize_index`.
- MLIR accepts inherent attributes when it creates an operation and converts them into properties, so builders could set them as attributes.

### E-5. Fields named `name` become `_name`

- 26 accessors and parameters are called `_name`, for example `memref::get_global(context, result, _name, location)`, because `name` is in `RESERVED_NAMES` (line 8 of `macro/src/dialect/utility.rs`) to avoid the generated `name()` function.
- A leading underscore means unused in Rust. Renaming the generated function, for example to `operation_name()`, would free the name.

### E-6. No way to add discardable attributes

- The hand-written `func::func` and `llvm::func` accept extra attributes such as `llvm.emit_c_interface`.
- Typed builders have no passthrough for them, and typed operations do not expose `OperationMutLike`.

## Open generator defects

All of these were checked against the code at the version above.

- D-1. For `SameOperandsAndResultType`, the setter of a variadic first operand indexes `[0]` to read the result type and panics on an empty slice (line 226 of `macro/src/dialect/generation/operation_builder.rs`). Since unnamed operands are kept, an unnamed variadic first operand reaches this path too.
- D-2. An optional attribute under `FirstAttrDerivedResultType` adds results on every setter call (lines 244 to 264 of the same file). Setting it twice gives two sets of results, and never setting it gives none.
- D-3. The `result_type` local in those setters uses the caller's naming context (lines 234 and 259), so a field named `result_type` breaks compilation with E0308.
- D-4. Under `SameVariadicOperandSize` and `SameVariadicResultSize`, getters of optional elements return an iterator while declaring a `Result` (line 56 of `macro/src/dialect/generation/element_accessor.rs`), so they fail to compile with E0308. No bundled operation hits this.
- D-5. `build()` calls `expect` when MLIR cannot create the operation, for example when type inference fails (line 298 of `operation_builder.rs`). The hand-written helpers behave the same, but a default API would be better off fallible.

## Cost and packaging

- C-1. Build time is not a blocker. The builders are generic, so code is generated only for those a crate uses.

  | Command                 | Without `ods-dialects` | With `ods-dialects` |
  | ----------------------- | ---------------------: | ------------------: |
  | `cargo check -p melior` |                    1 s |                 8 s |
  | `cargo build -p melior` |                  0-1 s |                11 s |

  The `cargo build` numbers are the same for the dev and release profiles. Per-dialect features would be an optimization, not a requirement.

- C-2. Tests need porting. The hand-written modules have 75 tests and 85 snapshot files, while `melior/src/dialect/ods.rs` has 16 tests.
- C-3. The `helpers` feature requires `ods-dialects` but mixes both APIs, so it needs rebasing onto the generated one.

## Already at parity

- Successor safety: generated successor constructors are `unsafe` (76 generated functions), with the same safety contract as the hand-written `cf::br`, `cf::cond_br`, and `cf::switch`.
- Segment sizes: builders write the operand and result segment size arrays since #922.
- Unnamed fields: unnamed results are kept since #974 and #1018, and unnamed operands with `bug/unnamed-operand`.
- Completeness: several generated constructors cover more than the hand-written ones. For example, `scf::r#for` takes initial arguments and results, which the hand-written one lacks.

## Suggested order

1. B-1 (inference), B-2 together with E-5 (typed attributes and naming), and D-1 to D-3. These are generator work with no API break.
2. E-1, E-2, E-3, and E-6 (ergonomics), with E-4 (properties) folded into the attribute work.
3. B-3: decide whether the non-op `llvm` items stay hand-written or get generated.
4. B-4, C-2, and C-3: deprecate the hand-written functions, port the tests, the README, and `helpers`, and then make `ods-dialects` a default feature.
