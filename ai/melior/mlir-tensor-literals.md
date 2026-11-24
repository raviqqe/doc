---
description: An AI audit of a fix to the MLIR parser that rejects tensor literals whose number of values does not match their types, a memory-safety defect found while auditing Melior.
---

# Audit of an MLIR parser fix for miscounted tensor literals

<!-- cspell: ignore canonicalizer llvmir melior memref nullptr uniquing xcomplex -->

- Date: 2026-11-23
- Model: Claude Opus 5.5
- Repository: [`llvm/llvm-project`](https://github.com/llvm/llvm-project)
- Version: local branch `mlir-dense-literal-element-count` at `5455a885d`, two commits on top of `main` at `0e3c188e8`

## Summary

- MLIR's parser accepts a parenthesized pair such as `(1, 2)` in dense and sparse literals of any element type and counts its elements as separate values. Only complex element types had their value counts checked, so `dense<(1, 2)> : tensor<5xi32>` became an attribute with two values for five elements. Builds with assertions abort on it. Builds without them, such as the Homebrew LLVM that Melior links, create the attribute, and the printer, folders and every other reader then read past its data. The defect was found while auditing Melior, where `Attribute::parse` and `Module::parse` reach it from safe Rust, as its only Critical finding (MEL-47).
- The fix rejects literals whose number of values is neither one nor the element count of their type, with the same diagnostic that the parser already emits for complex literals. It closes the out-of-bounds reads for every non-complex literal the audit could build, rejects nothing that MLIR prints or that any file in `llvm-project` uses, and adds no new abort to builds with assertions.
- Two audit rounds found no Critical, High or Medium findings. Of three Low findings, the inaccurate commit message (L2) is resolved. The other two are older count defects outside the fix: complex literals of zero-element types (L1) and element counts that overflow 64 bits (L3). L3 still crashes MLIR passes from safe Melior code.

## Scope and method

- The audit covers the diff against `main` in `mlir/lib/AsmParser/AttributeParser.cpp` and `mlir/test/IR/invalid-builtin-attributes.mlir`, the callers of `TensorLiteralParser::getAttr` (`parseDenseElementsAttr` and `parseSparseElementsAttr`; the class is local to its file), the code that consumes what it builds (the `DenseElementsAttr::get` overloads, `DenseStringElementsAttr`, attribute uniquing and the printer), the commit message, related upstream issues and pull requests, and the effect on Melior.
- Two `mlir-opt` binaries were compared: one built from the branch with assertions enabled, and Homebrew LLVM 23.1.6, an older release built without assertions. No unpatched build with assertions was kept, so its behavior comes from the build and test logs of the implementation and from reading the code.
- Each input ran in its own process. The inputs are quoted in the findings.
- The first round audited the first commit. The second round audited the branch after the review fixes, reran all 35 inputs of the first round and the added tests on the rebuilt binary, and checked the final commit message.

## Severity

| Severity      | Meaning                                                         |
| ------------- | --------------------------------------------------------------- |
| Critical      | Arbitrary memory reads or writes from IR text                   |
| High          | Memory errors or crashes from IR text under realistic use       |
| Medium        | Wrong results, aborts or contract violations with limited reach |
| Low           | Edge cases, panics and inaccuracies                             |
| Informational | Tests, documentation and hygiene                                |

## Overview

| Severity      | Open | Resolved |
| ------------- | ---- | -------- |
| Critical      | 0    | 0        |
| High          | 0    | 0        |
| Medium        | 0    | 0        |
| Low           | 2    | 1        |
| Informational | 8    | 0        |

## The defect

- `TensorLiteralParser::parseElement` accepts a parenthesized pair for any element type, and pairs nest, so `((1, 2), 3)` adds three values.
- `TensorLiteralParser::getAttr` compared the number of parsed values with the type only for complex element types. For other element types it called `DenseElementsAttr::get` or `DenseStringElementsAttr::get` directly.
- The precondition of `DenseElementsAttr::get`, one value or one value per element (`hasSameNumElementsOrSplat` in `mlir/lib/IR/BuiltinAttributes.cpp`), is only an assertion, and the string variant has none.

Results on LLVM 23.1.6 without assertions:

| Input                                                        | Result                                                                                    |
| ------------------------------------------------------------ | ----------------------------------------------------------------------------------------- |
| `dense<(1, 2)> : tensor<5xi32>`                              | Two values for five elements; elements 2 to 4 are heap bytes                              |
| `dense<((1, 2), 3)> : tensor<5xi32>`                         | Three values for five elements                                                            |
| `dense<(1, 1)> : tensor<5xi32>`                              | A valid splat, because equal values collapse while uniquing                               |
| `dense<("a", "bb")> : tensor<5x!llvm.ptr>`                   | String storage; any access or printing segfaults                                          |
| `memref.global "private" @g : memref<5xi32> = dense<(1, 2)>` | The type is implicit; the operation verifies and reads heap bytes                         |
| `tensor<5xi32, dense<(1, 2)> : tensor<5xi32>>`               | A type encoding that reads heap bytes                                                     |
| `dense<[(1, 2), (3, 4)]> : tensor<2xi32>`                    | Silently `[1, 2]`: wrong, but in bounds                                                   |
| `sparse<(0, 1), 5> : tensor<4x4x4xi32>`                      | The sparse verifier reads the indices out of bounds while parsing and prints a heap value |
| Printing `dense<(1, 2)> : tensor<5xi32>`                     | `dense<[1, 2, -993741000, 117, -993709976]> : tensor<5xi32>`                              |

Builds with assertions abort on all of these with `Assertion failed: (hasSameNumElementsOrSplat(type, values))`, except the string literal, which parses and aborts once the printer reads it.

Melior can't detect these attributes itself: the C API exposes no size of their stored data, and MLIR's printer reads the same bytes before Melior is involved. Melior 0.30.0 documents that untrusted IR text must not be parsed ([#1044](https://github.com/mlir-rs/melior/pull/1044)).

## The fix

```diff
   } else if (storage.size() > 1 &&
              storage.size() != static_cast<size_t>(type.getNumElements())) {
     p.emitError(loc) << "parsed " << storage.size() << " elements, but type ("
                      << type << ") expected " << type.getNumElements()
                      << " elements";
     return nullptr;
   }
```

The branch attaches to the existing complex check in `getAttr`, so it applies to every non-complex element type. The tests in `mlir/test/IR/invalid-builtin-attributes.mlir` cover four inputs:

- `dense<(1, 2)> : tensor<5xi32>`, with fewer values than elements
- `dense<[(1, 2), (3, 4)]> : tensor<2xi32>`, with more values than elements
- `dense<("a", "bb")> : tensor<5x!foo.bar>`, with string elements, which have no assertion
- `sparse<(0, 1), 5> : tensor<4x4x4xi32>`, with sparse indices, which report `parsed 2 elements, but type ('tensor<1x3xi64>') expected 3 elements`

The commit message to submit, which LLVM's squash merge takes from the pull request title and description:

```text
[mlir][AsmParser] Check element counts of non-complex tensor literals

The parser accepted a parenthesized literal for any element type and
parsed its elements as separate values, so a literal with a non-complex
element type could have a different number of values from the elements
of its type. For example, `dense<(1, 2)> : tensor<5xi32>` has two values
for five elements. Builds with assertions abort in
`DenseElementsAttr::get`, while builds without them create the
attribute, and any read of its third or later element, including by the
printer, goes out of bounds. For sparse elements such as
`sparse<(0, 1), 5> : tensor<4x4x4xi32>`, the verifier reads the indices
out of bounds while parsing.

Reject literals whose number of parsed values is neither one nor the
number of elements of their types, as the parser already does for
complex element types. The check is exactly the precondition that
`DenseElementsAttr::get` asserts for integer and floating-point
elements, so every such literal that builds with assertions accepted is
still accepted. String elements have no such assertion, so the check
newly rejects string literals whose values were truncated, collapsed
into splats or read out of bounds.
```

## Findings

### Low

#### L1: Complex literals of zero-element types still violate the precondition of `DenseElementsAttr::get`

- Where: `mlir/lib/AsmParser/AttributeParser.cpp:578`, `bool isSplat = shape.empty() && type.getNumElements() != 0;`, which the fix doesn't change. The new check at `:591` skips complex element types.
- Problem: when a literal has no list shape and its type has no elements, neither complex check runs, so nested pairs reach `DenseElementsAttr::get` with more than one value for zero elements.
- Evidence:
  - The patched build with assertions aborts on `"foo"() {a = dense<((1.0, 2.0), (3.0, 4.0))> : tensor<0xcomplex<f32>>} : () -> ()` with `Assertion failed: (hasSameNumElementsOrSplat(type, values)), function get, file BuiltinAttributes.cpp, line 951.` The `complex<i32>` variant fails the same assertion at line 929.
  - It accepts `dense<((1, 2), 3)> : tensor<0xcomplex<i32>>` and drops the `3`.
  - LLVM 23.1.6 accepts the first input and prints `dense<> : tensor<0xcomplex<f32>>` while storing 16 bytes for zero elements.
- Impact: builds with assertions abort, which matters to fuzzers, debug builds and the language server. Builds without assertions keep data longer than the type, and by reading `mlir/lib/IR/AttributeDetail.h:70-72`, such an attribute compares unequal to `dense<>` although both print the same. Nothing reads out of bounds, because readers iterate over zero elements. The defect predates the fix and lies outside its title, but it is the last count gap left in `getAttr`.
- Recommendation: compute `isSplat` as `shape.empty() && !storage.empty()`. For types with elements it is the same condition, because an empty literal already fails at `:565`. It rejects the inputs above and keeps both `dense<> : tensor<0xcomplex<f32>>` and `dense<(1, 2)> : tensor<0xcomplex<f32>>`, the latter matching the accepted `dense<1> : tensor<0xi32>`. The change is untested and needs a test.
- Status: open, left for a separate change.

#### L2: The commit message misstated the behavior of string literals and of builds without assertions

- Where: the message of the first commit.
- Problem: it claimed that builds with assertions abort as soon as string attributes with a wrong count are read, and that every reader of an attribute with a wrong count goes out of bounds. Unpatched builds accept and print the following without any out-of-bounds read, and the fix now rejects them:
  - `dense<("a", "a")> : tensor<5x!foo.bar>` becomes the splat `dense<"a">`, because equal strings collapse in `DenseStringElementsAttrStorage::getKey` (`mlir/lib/IR/AttributeDetail.h:177-182`).
  - `dense<[("a", "b"), ("c", "d")]> : tensor<2x!foo.bar>` is truncated to `dense<["a", "b"]>`.
  - `dense<("a", "bb")> : tensor<0x!foo.bar>` becomes `dense<>`.
- By reading, builds with assertions behave the same: the string builder has no count assertion, and the printer (`mlir/lib/IR/AsmPrinter.cpp:2768-2773`) and the element iterator (`mlir/lib/IR/BuiltinAttributes.cpp:601-603`) only index below the element count.
- Out-of-bounds reads also need fewer values than elements, not all equal: `dense<[(1, 2), (3, 4)]> : tensor<2xi32>` reads in bounds, and `dense<(1, 1)> : tensor<5xi32>` is a valid splat.
- Impact: none on the code, but reviewers relying on the claim that nothing is newly rejected would have been misinformed.
- Status: resolved by the message in [the fix](#the-fix), which the second round checked against the inputs above.

#### L3: Element counts that overflow 64 bits wrap in builds without assertions

- Where: `getNumElements` in `mlir/lib/IR/BuiltinTypeInterfaces.cpp:83-94` asserts on overflow with assertions and multiplies unchecked without them; `parseElementsLiteralType` in `mlir/lib/AsmParser/AttributeParser.cpp:1171-1192` only checks that the shape is static; the new comparison at `:592` uses the wrapped count.
- Problem: a shape whose element count overflows `int64_t` gets a wrapped count, and literals are checked and read against it.
- Evidence:
  - LLVM 23.1.6 accepts `dense<"0x0100000002000000"> : tensor<3x6148914691236517206xi32>` and prints `dense<[[1, 2]]>`, because 3 × 6148914691236517206 wraps to 2.
  - With that constant and a `tensor.extract` at `[1, 0]`, `mlir-opt --canonicalize` segfaults in `DenseElementTypeInterfaceTraits::Model<IntegerType>::convertToAttribute`, called from `tensor::ExtractOp::fold`.
  - From Melior 0.30.0 under `#![forbid(unsafe_code)]`, `Module::parse` accepts the same function, `verify()` returns true, and a `PassManager` running the canonicalizer segfaults in 3 of 3 runs. Melior's own accessors are safe: `DenseElementsAttribute::try_from` rejects the attribute with `countable type expected`.
  - The patched build with assertions aborts on `dense<(1, 2)> : tensor<3x6148914691236517206xi32>` and on the hex literal with `integer overflow in element count computation`. For the pair, it now aborts inside the new check where it used to abort in `hasSameNumElementsOrSplat`. By reading, a patched build without assertions compares the pair with the wrapped count 2 and accepts it.
  - Dynamic shapes are safe: `dense<(1, 2)> : tensor<?xi32>` and `sparse<(0, 1), 5> : tensor<?x4xi32>` fail with `elements literal type must have static shape`, and the types of sparse indices and values are built from parsed shapes, which are always static.
- Impact: malformed literal text, even without parentheses, still reads out of bounds in builds without assertions after the fix. The fix neither introduces nor worsens it.
- Recommendation: reject types whose element count overflows, for example with `ShapedType::tryGetNumElements` in `parseElementsLiteralType` or in the verifiers of tensor and vector types.
- Status: open, left for a separate change.

### Informational

#### I1: The check counts values but not their structure

- These inputs are still accepted, as builds with assertions accept them, and are memory-safe:
  - `dense<(1, 2)> : tensor<2xi32>`, read as `[1, 2]`
  - `dense<(((1, 2), (3, 4)), 5)> : tensor<5xi32>`, read as `[1, 2, 3, 4, 5]`
  - `sparse<(0, 1), 5> : tensor<4x4xi32>`
  - `dense<(true, false)> : tensor<2xi1>`
  - `dense<[((1, 2), 3), 4]> : tensor<2xcomplex<i32>>`
- List literals are checked in effect: every leaf adds at least one value and the shape must match, so a count equal to the element count implies that there are no pairs. Only a top-level parenthesized literal slips through.
- A stricter fix could record whether a pair was parsed and reject pairs for non-complex element types. No file in `llvm-project` uses these forms.

#### I2: Builds without assertions reject some inputs they used to accept

- `dense<(1, 1)> : tensor<5xi32>` and the string literals in L2 produced well-formed attributes in builds without assertions and are now rejected. Nothing in the tree produces them, and the printer never emits parentheses for non-complex elements.

#### I3: Every path to the check was exercised

- The patched build rejects pairs in a vector type, a tensor type encoding, `mxi8` elements, `i1`, `index`, a zero-element type, sparse values, sparse indices, an implicit type through `memref.global`, a string type with integer tokens, and a float token in an integer literal. LLVM 23.1.6 produces garbage or out-of-bounds data for each, and for the zero-element type it silently gives `dense<>`.
- These are still accepted:
  - hex literals, which return before the check (`AttributeParser.cpp:552-554`)
  - `dense<>`, `dense<[]>` and splats of zero-element types
  - the type-first syntax, including `mxi8`, which goes through `parseDenseElementsAttrTyped` instead of `TensorLiteralParser`
  - string lists, splats and hex strings
  - sparse literals, including complex values
  - nested lists and scalar tensors
  - `memref.global` with a complex splat
- `dense_resource` and `array<>` don't use `TensorLiteralParser` and were not audited further.
- Between the rounds, the condition changed from `!storage.empty() && storage.size() != 1` to `storage.size() > 1`, which is the same for `size_t`. All 35 inputs of the first round gave the same first diagnostic or output line on the rebuilt binary.

#### I4: The tests fail without the fix and pass with it

- `llvm-lit` passes all 150 tests in `mlir/test/IR` and `mlir/test/Dialect/MemRef` before and after the fix.
- The added cases, run with `-split-input-file -verify-diagnostics`, pass on the patched build and fail on LLVM 23.1.6 with `expected error ... was not produced`. There the sparse case also reports `sparse index #0 is not contained within the value shape, with index=[0, 1, 515812468536]`, with a heap value as the third index.
- Review removed the cases for nested pairs, floating-point elements and `memref.global`, because they take the same path as the remaining ones.
- Optional additions: a zero-element type such as `dense<(1, 2)> : tensor<0xi32>`, the sparse values side such as `sparse<[[0, 0, 0]], (1, 2)> : tensor<4x4x4xi32>`, and `dense<(1, 1)> : tensor<5xi32>`.

#### I5: Diagnostics

- The counts were correct in every case tried. For sparse literals, the message names the type that the parser builds internally (`tensor<1x3xi64>` for indices and `tensor<1xi32>` for values), and for `memref.global` it names the converted tensor type. Both follow the existing messages for complex literals.
- The count check now runs before the checks of token kinds, so `dense<(1.5, 2)> : tensor<5xi32>` reports the count instead of `expected integer elements`. No existing test depends on the old order.

#### I6: No literal in the tree is affected

- A scan of `mlir/`, `flang/` and `clang/test/CIR` found 74 parenthesized literals. Every one with a non-complex element type is one of the new tests or a false positive of the scan, and the only one without an explicit type is complex (`mlir/test/Integration/Dialect/LLVMIR/CPU/test-complex-sparse-constant.mlir:9`).

#### I7: Upstream

- No upstream issue or pull request covers non-complex literals. [#133220](https://github.com/llvm/llvm-project/pull/133220) fixed [#132859](https://github.com/llvm/llvm-project/issues/132859) for complex element types only, and [#231876](https://github.com/llvm/llvm-project/pull/231876) and the fixes for [#228850](https://github.com/llvm/llvm-project/issues/228850) and [#159735](https://github.com/llvm/llvm-project/issues/159735) concern string tokens in integer literals.
- The open draft [#181385](https://github.com/llvm/llvm-project/pull/181385) separates `DenseStringElementsAttr` from `DenseElementsAttr` and changes the return types of `getAttr`, `getStringAttr` and the sparse path in the same file. Its hunks differ, and there is no semantic conflict, because the new check runs before the string branch and returns null.

#### I8: Effect on Melior

- Melior 0.30.0 requires `mlir-sys` 230.0.2 (LLVM 23), so the example of the defect in its README stays accurate until it moves to an LLVM with the fix, which is LLVM 24 unless the fix is backported. The example should then change, for example to the input of L3, and the warning should stay, since L1 and L3 remain.
- Melior's element accessors are bounded by `len()` (`mlirElementsAttrGetNumElements`, `melior/src/ir/attribute/dense_elements.rs:227`), so the excess data of L1 stays in bounds for Melior.

## Changes after review

- The condition was simplified to `storage.size() > 1`.
- The cases for `memref.global`, nested pairs and floating-point elements were removed, and the sparse case moved next to `@invalid_sparse_indices`.
- The commit message was corrected (L2), and its title says "tensor literals" instead of "dense literals", since the fix also covers sparse literals.

## Recommendations

1. Submit the fix with the message in [the fix](#the-fix) as the pull request title and description.
2. Fix L3 upstream by rejecting types whose element count overflows. It is the remaining way for literal text to read out of bounds in builds without assertions.
3. Fix L1 upstream with the change to `isSplat` and a test.
4. Optionally add the tests in I4.
5. In Melior, keep the warning about untrusted IR text, and update its example when moving to an LLVM with the fix.
