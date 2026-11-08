---
description: An AI security and correctness audit of Melior, the Rust bindings to the MLIR C API, covering memory-safety gaps reachable from its safe API and bugs in the procedural macro that generates its dialect bindings.
---

# Security audit of Melior

<!-- cspell: ignore entrancy landingpad lossily melior repr -->

- Date: 2026-11-08
- Model: Claude Opus 5.5
- Repository: [`mlir-rs/melior`](https://github.com/mlir-rs/melior)
- Version: `main` at `e6407a664`

## Summary

- Safe code can still cause use-after-free in four areas: rewrite patterns that erase IR and then report no match (H7), iterators and walks that fetch the next element in advance (M7), callbacks that are re-entered or freed while running (M8), and functions that don't check the kinds of their arguments or whether operations are attached to blocks (M1, M2). All of them except M7 segfault in reproductions; M7 is known from reading the code.
- Since the first audit at `10c5a6ebe` (version 0.28.2), one Critical, eight High and five Medium findings were fixed, three of the Medium ones in the macro. The largest changes tied IR handles to their owners and contexts (#999, #1000), made mutable operation references exclusive (#1001), validated indices and dense element attributes (#1005, #1006), and bounded callbacks that MLIR keeps (#1012).
- Most remaining memory errors need code that misuses an API in a way its types don't prevent, such as reading a call site from a location of another kind, moving a detached operation, or emitting a diagnostic from a diagnostic handler. Tools that inspect untrusted IR are the most exposed: M1 turns an unchecked accessor on parsed IR into a crash, and M5 aborts the process when a diagnostic handler prints IR that isn't valid UTF-8.
- The procedural macro `melior-macro` has four Low findings. One of them (X-L4) is new: the change that names unnamed ODS results (#974, #1018) still drops unnamed operands, so a named operand after an unnamed one reads the wrong operand, and 24 upstream operations get builders that can't add their operands.

## Scope and method

- The audit covers both packages in the repository: `melior`, the bindings, and `melior-macro`, the procedural macro that generates dialect bindings from ODS TableGen definitions. The raw bindings in `mlir-sys` are out of scope.
- The first round read every source file of both packages at `10c5a6ebe`. Later rounds read every line changed since the previous round. This round covers #1015, #1016, #1019, #974 and #1018.
- Every finding was reproduced with a safe Rust program unless it is marked "by source". The programs ran against Homebrew LLVM 23.1.4, a release build in which MLIR's assertions are off, so violated preconditions are undefined behavior rather than assertion failures.
- Memory errors were confirmed under Guard Malloc (`libgmalloc`) on macOS, which turns a use-after-free into a deterministic segmentation fault.
- MLIR's behavior was checked against its C API and C++ sources in `llvm-project` at `d19748bc3c0d`.
- Reproduction programs are referred to as `rNN`. The ones that show memory errors are included in this report. In them, `context()` creates a context with all dialects loaded and unregistered dialects allowed.

## Severity

| Severity      | Meaning                                                                                 |
| ------------- | --------------------------------------------------------------------------------------- |
| Critical      | Memory corruption reachable from ordinary safe code                                     |
| High          | Memory corruption reachable from safe code through common usage of an API               |
| Medium        | Memory corruption, a data race or a process abort reachable from less common safe usage |
| Low           | Wrong results, leaks or panics in safe APIs                                             |
| Informational | Hardening, documentation and maintenance issues                                         |

## Overview

| Package        | Critical | High | Medium | Low | Informational |
| -------------- | -------- | ---- | ------ | --- | ------------- |
| `melior`       | 0        | 1    | 7      | 4   | 5             |
| `melior-macro` | 0        | 0    | 0      | 4   | 4             |

## Fixed findings

| ID   | Finding                                                                                         | Fixed by              |
| ---- | ----------------------------------------------------------------------------------------------- | --------------------- |
| C1   | IR handle lifetimes weren't tied to their owners or contexts                                    | #999, #1000           |
| H1   | MLIR kept strings borrowed from Rust after the borrows ended                                    | #1004                 |
| H2   | Indices and slice lengths went to MLIR unchecked                                                | #1005                 |
| H3   | Dense elements attributes didn't validate counts, widths or kinds                               | #1006                 |
| H4   | Mutable operation handles were copyable and convertible into owned operations                   | #982, #1001           |
| H5   | Successor blocks were stored without a lifetime                                                 | #1007 (made `unsafe`) |
| H6   | The greedy rewrite driver erased operations behind shared borrows                               | #1013                 |
| H8   | Callbacks that MLIR keeps had no `Send` or `'static` bounds                                     | #1012                 |
| H9   | Shared block handles gave mutable operation handles to the operation-level rewrite drivers      | #1015 (made `unsafe`) |
| M4   | String references mishandled empty strings and trailing NULs                                    | #1019                 |
| M9   | Walk callbacks couldn't use data from the caller's context (a regression from #999)             | #1009                 |
| X-M1 | Builders of operations with attribute-sized segments never set segment sizes                    | #922                  |
| X-M2 | Accessors of empty variadic groups overflowed in debug builds                                   | #954                  |
| X-M3 | Accessors of same-size variadic groups computed wrong group lengths                             | #966                  |
| —    | Leaks of frozen pattern sets, closures and external pass state, and panics in pattern callbacks | #956, #980, #982      |

## `melior`

### High

#### H7: Erasing rewriter methods are safe, and a pattern that rewrites and then reports no match makes MLIR retry an erased root

- Where: `RewriterBase::erase_op`, `erase_block`, `replace_op_with_values`, `replace_op_with_operation`, `inline_block_before` and `merge_blocks` (`melior/src/ir_rewriter.rs:156` to `:281`), and `create_op_rewrite_pattern` (`melior/src/rewrite_pattern.rs:125`).
- Problem: the methods take copyable handles and are safe to call, so any other copy of an erased operation or block dangles. MLIR also requires a pattern to report success if and only if it changed the IR. A callback that erases or replaces its matched operation and returns `false` makes the driver try the next pattern on the erased root.
- Impact: use-after-free from safe code in programs whose rewrite patterns erase or replace IR.
- Evidence: `r38` segfaults, including without Guard Malloc.

  ```rust
  for benefit in [2, 1] {
      set.add(create_op_rewrite_pattern(
          "arith.constant",
          benefit,
          &context,
          |operation, rewriter| {
              let rewriter = rewriter.as_rewriter_base();
              let replacement = rewriter.clone_op(operation);
              rewriter.replace_op_with_operation(operation, replacement);
              false
          },
          &[],
      ));
  }
  ```

- Recommendation: report a pattern as matched whenever it changed the IR through the rewriter, and mark the erasing methods `unsafe` until they can consume exclusive handles. A draft fix doing both is open as #1014.

### Medium

#### M1: Functions don't check the kinds of their arguments

- Where: location accessors such as `Location::call_site_callee` and `file_line_col_range_start_line` (`melior/src/ir/location.rs:126`, `:141`), `Type::float_width` (`melior/src/ir/type.rs:138`), `AffineExpr::lhs`, `rhs`, `dim_position` and `symbol_position` (`melior/src/ir/affine_expr.rs:78` to `:98`), `AffineMap::single_constant_result` (`melior/src/ir/affine_map.rs:166`), and `IntegerAttribute::new` and `FloatAttribute::new` (`melior/src/ir/attribute/integer.rs:19`, `float.rs:19`).
- Problem: the C functions behind them assume a specific kind of object and cast without checking. Several of these preconditions are only written in doc comments.
- Impact: type confusion or a null dereference from safe code. Code that inspects parsed IR, such as reading the callee of every call site location, crashes on input of another kind.
- Evidence:
  - `r11`: `call_site_callee` on an unknown location segfaults, and `file_line_col_range_start_line` returns `usize::MAX`.
  - `r12`: `float_width` on an index type segfaults.
  - `r22`: `AffineExpr::dim(&context, 0x4141_4141).lhs()` segfaults while dereferencing a pointer derived from the dimension position, and `single_constant_result` on an empty map segfaults.
  - `r23`: `IntegerAttribute::new` with an `f32` type prints `1 : f32`.
- Recommendation: check kinds with the C API's `*IsA*` predicates and return `Option` or `Result`, or move the methods onto typed wrappers such as a call site location type.

#### M2: Structural operations on detached or malformed IR dereference null

- Where: `OperationMutLike::move_after` and `move_before` (`melior/src/ir/operation/operation_like.rs:508`, `:516`), `OperationLike::is_before_in_block` (`:427`), `RewriterBase::insertion_block` and `block` (`melior/src/ir_rewriter.rs:117`, `:122`), and `Module::try_from` (`melior/src/ir/module.rs:90`).
- Problem: these functions assume that operations are in blocks, that a rewriter has an insertion point, or that a `builtin.module` operation has a body region.
- Impact: null dereferences and out-of-bounds region access from safe code.
- Evidence: `r08`, `r10`, `r28` and `r41` segfault.

  ```rust
  let block = Block::new(&[], &[]).unwrap();
  let anchor = block.append_operation(OperationBuilder::new("foo.a", location).build().unwrap());
  let mut detached = OperationBuilder::new("foo.b", location).build().unwrap();

  detached.move_after(anchor);
  ```

  ```rust
  let rewriter = IrRewriter::new(&context);

  rewriter.as_rewriter_base().insertion_block().argument_count();
  ```

  ```rust
  let module = Module::try_from(OperationBuilder::new("builtin.module", location).build().unwrap()).unwrap();

  module.body().append_operation(OperationBuilder::new("foo.bar", location).build().unwrap());
  ```

- Recommendation: check for a parent block before moving or comparing operations, return `Option<BlockRef>` from the rewriter getters, and check the region and block counts in `Module::try_from`.

#### M3: `ExecutionEngine::new` ignores a null result

- Where: `melior/src/execution_engine.rs:15`.
- Problem: `mlirExecutionEngineCreate` returns null when translation to LLVM IR fails, and the constructor wraps it anyway.
- Impact: any later method call dereferences null.
- Evidence: `r18` builds an engine for a module that isn't lowered to the LLVM dialect, and `lookup` segfaults.
- Recommendation: return `Result<Self, Error>`.

#### M5: IR that isn't valid UTF-8 aborts the process inside diagnostic handlers

- Where: `print_callback` (`melior/src/utility.rs:76`) and the `Display` implementations that use it.
- Problem: MLIR string escapes can produce symbol names that aren't valid UTF-8 from valid UTF-8 source. Printing them makes `Display` return an error, and a `{diagnostic}` format inside a diagnostic handler then panics inside an `extern "C"` function, which aborts.
- Impact: denial of service for any tool that parses untrusted MLIR text and prints diagnostics.
- Evidence: `r39` parses the following with a handler that prints each diagnostic, and aborts.

  ```mlir
  func.func private @g()
  func.func @f() { func.call @"g\FE"() : () -> () return }
  ```

- Recommendation: convert printed chunks with lossy UTF-8 conversion in the print callbacks.

#### M6: Pass registration is safe to call but not synchronized

- Where: the `register_*` functions generated by `macro/src/pass.rs`, `register_all_passes` (`melior/src/utility.rs:28`), and the registry readers `parse_pass_pipeline` (`:36`) and the transform interpreter pass.
- Problem: MLIR keeps registered passes and pipelines in global hash maps with no lock (`mlir/lib/Pass/PassRegistry.cpp`). The `Once` in `register_all_passes` only prevents that function from running twice. It doesn't serialize it with the individual registration functions, with pipeline parsing, which looks passes up, or with the transform dialect's `apply_registered_pass`, which looks passes up while a pass manager runs.
- Impact: a data race on the registry when safe functions are called from different threads, for example from parallel tests.
- Evidence: by source.
- Recommendation: serialize registration and pipeline parsing behind one global lock in Melior. Covering pass runs as well would also need the lock during `PassManager::run`, which deadlocks if Rust code inside a pass registers passes or parses a pipeline. Alternatively, mark the registration functions `unsafe`.

#### M7: Iterators and walks fetch the next element in advance

- Where: the block and region iterators (TODOs at `melior/src/ir/block.rs:220` and `melior/src/ir/region.rs:140`) and the walks, which run on MLIR's own walk.
- Problem: each iterator reads the next operation or block before yielding the current one, as MLIR's early increment ranges do. Erasing or moving that next element during iteration leaves the iterator holding a dangling pointer.
- Impact: use-after-free from safe code that erases or moves IR while iterating.
- Evidence: by source.
- Recommendation: document that only the current element may be erased or moved, and enforce it with borrows where possible, for example by tying the next element to a borrow of the iterator.

#### M8: Callbacks can be re-entered or freed while they are running

- Where: the trampolines of diagnostic handlers and rewrite patterns (`melior/src/context.rs:116`, `melior/src/rewrite_pattern.rs:151`) and `Context::detach_diagnostic_handler` (`melior/src/context.rs:138`).
- Problem: each call creates a new `&mut` to the closure from a raw pointer, with no guard against re-entry. A diagnostic handler that emits a diagnostic, or a pattern that runs a driver with its own pattern set, gets two live mutable references to the same closure. A handler that detaches itself frees the closure that is running. Requiring `'static` callbacks (#1012) doesn't prevent either, since a callback can capture a leaked `&'static Context`.
- Impact: aliasing mutable references and use-after-free from safe code.
- Evidence: `r44`, `r45` and `r54` segfault under Guard Malloc. Without it, they read freed memory: `r54` prints a garbage value from a reallocated buffer.

  ```rust
  let context: &'static Context = Box::leak(Box::new(context()));
  let mut log = Vec::<String>::with_capacity(1);

  context.attach_diagnostic_handler(move |diagnostic| {
      log.push(diagnostic.to_string());
      let first = &log[0];

      if log.len() < 4 {
          let _ = Module::parse(context, "bar");
      }

      println!("first message: {first}");
      true
  });

  let _ = Module::parse(context, "foo");
  ```

  ```rust
  let context: &'static Context = Box::leak(Box::new(context()));
  let mut module = Module::parse(context, "module { module {} }").unwrap();
  let cell = Rc::new(OnceCell::<FrozenRewritePatternSet>::new());
  let set = RewritePatternSet::new(context);
  let mut log = Vec::<u64>::with_capacity(1);

  set.add(create_op_rewrite_pattern(
      "builtin.module",
      1,
      context,
      {
          let cell = cell.clone();

          move |_, _| {
              log.push(log.len() as u64);
              let first = &log[0];

              if log.len() < 3
                  && let Some(frozen) = cell.get()
              {
                  let mut inner = Module::parse(context, "module { module {} }").unwrap();
                  let _ = apply_patterns_and_fold_greedily(&mut inner, frozen, &GreedyRewriteDriverConfig::new());
              }

              println!("first entry: {}", *first);
              false
          }
      },
      &[],
  ));
  let _ = cell.set(set.freeze());

  let _ = apply_patterns_and_fold_greedily(&mut module, cell.get().unwrap(), &GreedyRewriteDriverConfig::new());
  ```

- Recommendation: add a re-entrancy flag to each trampoline that makes an inner call fail or abort, and defer detaching a running handler until it returns.

### Low

#### L1: Unconsumed builders and patterns leak

- Where: `OperationBuilder` (`melior/src/ir/operation/builder.rs`), `RewritePattern` (`melior/src/rewrite_pattern.rs`) and `Pass` (`melior/src/pass.rs`).
- Problem: none of them implements `Drop`. An operation builder that is never built leaks its operation state's arrays and its regions, with all the IR in them. A pattern that is never added to a set leaks with its callback. The C API has no function to destroy a pass or a pattern; only pass managers and pattern sets free them.
- Status: `Pass` is `#[must_use]` since #1016, so an unused pass warns but still leaks.
- Recommendation: keep the builder's lists in Rust until `build`, so that dropping it frees them, and free an unused pattern by adding it to a temporary pattern set. Freeing an unused pass needs a destroy function in the C API.

#### L2: Wrong results instead of errors

- `BlockLike::insert_operation` (`melior/src/ir/block/block_like.rs:103`) wraps around past the end: inserting at position 3 into a block of two operations inserts first (`r36`).
- `MemRefType::strides_and_offset` (`melior/src/ir/type/mem_ref.rs:95`) ignores failure of the C function and returns `([0, 0], 0)` for a layout without strides (`r36`).
- `AffineMap::minor_identity(1, 4)` returns `(d0) -> (d0)`, and `major_sub_map(0)` returns a null map (`r22`).
- `cf::cond_br` (`melior/src/dialect/cf.rs:59`) derives operand segment sizes from the successors' argument counts, so operands that don't match fail verification (`r36`).
- `RankedTensorType::new` (`melior/src/ir/type/ranked_tensor.rs:19`) casts dimensions of 2^63 and above to negative numbers, so `r48` builds `tensor<-1xi8>`.

#### L3: Panics in safe APIs

- `Module::parse` panics on input containing a NUL byte (`melior/src/ir/module.rs:30`).
- `OperationLike::previous_in_block` is `todo!()` (`melior/src/ir/operation/operation_like.rs:243`).
- `Location::fused_location` panics on an out-of-range index (`melior/src/ir/location.rs:166`).
- The dialect helpers call `expect("valid operation")` in 56 places.
- The element count of a dense elements attribute overflows and panics in debug builds for huge shapes (`melior/src/ir/attribute/dense_elements.rs:428`, `r48`).

#### L4: A pass nested under the wrong operation aborts the process

- Where: `OperationPassManager::add_pass` (`melior/src/pass/operation_manager.rs:30`).
- Problem: adding a module pass to a pass manager nested under `func.func` hits `report_fatal_error` in MLIR, which aborts the process (`r32`).
- Recommendation: compare the pass's operation name with the pass manager's before adding it, or document the abort.

### Informational

#### I1: Layout assumptions

The `Deref` implementations of `BlockRef`, `RegionRef` and `OperationRefMut`, and the `to_ref` methods of `OperationRef` and `ContextRef`, transmute reference handles into their owned types (`melior/src/ir/operation.rs:163`, `:279`, `melior/src/ir/block.rs:179`, `melior/src/ir/region.rs:111`, `melior/src/context.rs:220`). They rely on both structs having the same layout, which neither type guarantees. `OperationBuilder::add_regions_vec` transmutes `Vec<Region>` into `Vec<ManuallyDrop<Region>>` (`melior/src/ir/operation/builder.rs:87`). #1003 made the handle types passed to C as arrays `#[repr(transparent)]`.

#### I2: Preconditions only in doc comments

Some safe functions state preconditions only in their documentation, such as a value being of a specific kind. M1 lists the ones that crash.

#### I3: Dynamic index sentinel collision

`GepIndex::Const(i32::MIN)` (`melior/src/helpers/llvm.rs:19`) collides with the value MLIR uses to mark dynamic indices, so that constant index is read as a dynamic one.

#### I4: Mismatches the types don't catch

- Invariant context lifetimes can't distinguish two contexts that are alive in the same scope, so IR from them can still be mixed.
- `PassManager::run` (`melior/src/pass/manager.rs:73`) accepts a module from any context. No memory error was found from this.
- `Error` has both `LocationCountMismatch` and the general `CountMismatch` (`melior/src/error.rs:22`, `:40`).

#### I5: The README's safety section is out of date

The section's example, invalidation of references returned by `Region::append_block`, has been a compile error since #1000, and the section doesn't mention H7, M1, M2, M7 or M8. An update (#1017) was closed without merging.

## `melior-macro`

### Low

#### X-L1: The `SameOperandsAndResultType` builder panics on an empty first variadic operand

- Where: `generate_same_operands_first_fn` (`macro/src/dialect/generation/operation_builder.rs:214`).
- Problem: the generated setter reads the result type from the first element of the first operand's slice (`:226`), so an empty slice panics.
- Evidence: `r46` panics with an index out of bounds.

#### X-L2: The setter of an optional `FirstAttrDerivedResultType` attribute adds results on every call

- Where: `generate_first_attr_derived_fn` (`macro/src/dialect/generation/operation_builder.rs:244`).
- Problem: optional attributes have no type state parameter, so calling the setter twice adds two sets of results, and never calling it leaves the operation without results.
- Evidence: `r46` builds an operation with two results instead of one.

#### X-L3: Getters of optional elements in `SameVariadic*Size` operations don't compile

- Where: `generate_element_accessor` (`macro/src/dialect/generation/element_accessor.rs`).
- Problem: the generated getter produces an iterator but declares `Result<Value, Error>`, so a dialect with such an operation fails with E0308. No bundled dialect has one.
- Evidence: by source.

#### X-L4: Unnamed operands are dropped before operand positions are computed

- Where: `Operation::collect_operands` (`macro/src/dialect/operation.rs:394`, the filter at `:405`).
- Problem: #974 and #1018 name unnamed results `result_<index>` with indices over all results, but operands without names are still filtered out before positions are computed. A named operand that follows an unnamed one therefore reads the wrong operand, and builders have no parameter for unnamed operands.
- Impact: wrong values from accessors in custom dialects. In the dialects Melior generates, 24 operations have unnamed operands, none followed by a named one, so their accessors are right but their builders can't add those operands. Examples are `affine.dma_start`, `affine.if`, `llvm.landingpad`, the `llvm.intr.*.with.overflow` intrinsics and 15 Arm SVE intrinsics.
- Evidence: `r55` defines `(ins I32, I64:$named)`, and `named()` returns the `i32` operand.
- Recommendation: give unnamed operands positional names as for results, and compute positions over all operands.

### Informational

#### X-I1: Accessor arithmetic can underflow on malformed operations

Typed operations only check the operation name on conversion, so for an unverified operation with too few operands, expressions such as `count() + 1 - length` (`macro/src/dialect/generation/element_accessor.rs:34`, `:41`) underflow. Debug builds panic, and release builds wrap and return errors.

#### X-I2: Build tracking and naming

`include_directory_env_vars` is read with `env::var` (`macro/src/dialect.rs:37`) without being tracked as a build input, so changing the variable leaves a stale expansion. `map_name` (`macro/src/utility.rs:10`) produces inconsistent names, such as `is_float_4_e_2_m_1_fn` next to `is_float8e4m3fn`.

#### X-I3: The build script runs `llvm-config` through a shell with an unquoted path

`macro/build.rs:38` runs `sh -c` with the `llvm-config` path interpolated unquoted, so an installation path containing spaces breaks the build.

#### X-I4: A generated local variable shadows fields of the same name

The setters of inferred result types bind a local variable named `result_type` (`macro/src/dialect/generation/operation_builder.rs:234`, `:259`) with a call site span, so an operand or attribute named `result_type` is shadowed by it. `r47` fails to compile with E0308. #947 fixed the same problem for `sizes` with a mixed site span.

## Reproduction status

| Result             | Reproductions                                                                                           |
| ------------------ | ------------------------------------------------------------------------------------------------------- |
| Segmentation fault | `r08`, `r10`, `r11`, `r12`, `r18`, `r22`, `r28`, `r38`, `r41`, `r44`, `r45`, `r54`                      |
| Abort              | `r32`, `r39`                                                                                            |
| Panic              | `r36`, `r46`, `r48`                                                                                     |
| Wrong result       | `r22`, `r23`, `r36`, `r46`, `r48`, `r55`                                                                |
| No longer compiles | `r01`, `r02`, `r06`, `r07`, `r09`, `r19`, `r20`, `r24`, `r37`, `r42`, `r43`, `r51`, `r52`, `r53`        |
| Fails to compile   | `r47` (X-I4)                                                                                            |
| Fixed at run time  | `r03`, `r04`, `r05`, `r13`, `r14`, `r15`, `r16`, `r17`, `r25`, `r26`, `r33`, `r34`, `r35`, `r40`, `r49` |
| No issue found     | `r21`, `r50` (diagnostic handlers run only on the context's thread)                                     |

`r43`, `r52` and `r53` no longer compile without `unsafe` since #1015. The other programs that no longer compile were fixed by the lifetime, bound and borrow changes listed in [fixed findings](#fixed-findings).

## Recommendations

1. Fix H7, the only remaining High finding, by finishing #1014.
2. Fix M8 and M7, the remaining use-after-free reachable from safe code without malformed IR, with re-entrancy guards in the callback trampolines and documented or enforced iteration rules.
3. Add kind and attachment checks for M1, M2 and M3. They are local validation changes like the ones in #1005 and #1006.
4. Convert printed IR lossily for M5, since it affects tools that process untrusted input.
5. Decide how far the registry lock for M6 should reach.
6. Fix X-L4 by naming unnamed operands positionally, as #974 did for results.
