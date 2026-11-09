---
description: An AI design study of how Melior can give exclusive mutable access to MLIR IR without runtime checks, comparing its handles with inkwell's and laying out compile-time designs that separate editing IR from freeing it.
---

# Exclusive mutation of IR in Melior without runtime checks

<!-- cspell: ignore badref melior segfaulted -->

- Date: 2026-11-08
- Model: Claude Opus 5.5
- Repository: [`mlir-rs/melior`](https://github.com/mlir-rs/melior)
- Version: `main` at `acd330ab8`

## Summary

- Melior's IR handles borrow their owners since #1000, which stops them from outliving the IR. The same borrows make exclusive mutable accessors impractical: an exclusive borrow of a block can't coexist with its arguments, results or sibling operations, and ordinary edits need those at the same time.
- Mutable accessors are therefore not exclusive today. Mutable operation handles come from shared block handles, which is why #1015 made the operation-level rewrite drivers `unsafe`.
- Only freeing IR invalidates other handles. A design that keeps edits on shared handles and requires exclusive access to the owner only for freeing, naming the targets through cursors or scoped closures, needs no runtime checks.
- inkwell avoids the conflict by not tying handles to their owners at all, and its safe API has use-after-free as a result.
- While checking this design, I found that `move_after` and `move_before` can move an operation into a block with another owner. The operation is then freed with that block while a handle to it stays usable, so safe code segfaults.

## Handles borrow their owners

A handle to anything inside a block is a Rust borrow of that block. `BlockLike::argument` returns a `BlockArgument<'c, 'a>` and `BlockLike::first_operation` returns an `OperationRef<'c, 'a>`, where `'a` is a shared borrow of the block. Nested handles carry the same `'a` all the way down: `OperationLike::region` returns a `RegionRef<'c, 'a>` and `RegionLike::first_block` returns a `BlockRef<'c, 'a>`.

This is what the owner lifetimes in #1000 added. Dropping or moving an owned block, region or module while a handle into it is alive is a compile error, where it was a use-after-free before.

## Current state

Verified on `main` at `acd330ab8`:

- `BlockLike::first_operation_mut(self)` and `terminator_mut(self)` take the block handle by value, and that handle is a shared, copyable reference. They can be called any number of times, alongside shared operation handles and values from the same block.
- `OperationMutLike::next_in_block_mut(&mut self)` and `parent_operation_mut(&mut self)` exclusively borrow only the handle they are called on. The sibling or parent they return can still be referenced elsewhere, for example by a walk that has already fetched it.
- `OperationRefMut` is neither `Copy` nor convertible into an owned `Operation` since #1001.
- Edits live on `OperationMutLike` and take `&mut self`: `set_attribute`, `move_after`, `move_before`, `set_operand`, `set_operands` and `replace_uses_of_with`. `remove_from_parent` is `unsafe`.

As a result, mutable handles aren't proof of exclusive access. #1015 made `apply_patterns_and_fold_greedily_with_op` and `walk_and_apply_patterns` `unsafe`, because both erase IR nested in an operation that other handles may still reference. The rewriter's erasing methods are still safe, and a draft (#1014) marks them `unsafe`.

## Why exclusive accessors are impractical

Making the `_mut` accessors take `&mut self` would make a mutable handle to one operation require an exclusive borrow of the whole block. Rust allows either many shared borrows of the block or one exclusive borrow, so code like the following stops compiling:

```rust
let argument = block.argument(0)?;              // shared borrow of `block`
let sibling = block.first_operation().unwrap(); // another shared borrow

let mut operation = block.terminator_mut().unwrap(); // would need `&mut block`

operation.set_operand(0, argument.into()); // error[E0502]: `block` is also borrowed as immutable
operation.move_after(sibling);             // same error
```

This is ordinary editing: pointing an operand at one of the block's arguments, or moving an operation next to a sibling. Neither frees anything, so the other handles stay valid. But a borrow can only lock the whole block, so "editing this one operation" becomes "nothing else from this block may be alive", which forbids far more than the edits that actually invalidate handles. The same conflict blocks splitting the block traits into shared and mutable ones.

## Moving operations across owners

`move_after` and `move_before` take the anchor as an `OperationRef<'c, 'a>` with the same `'a` as the moved operation. Owner lifetimes are covariant, so Rust shrinks the borrows of two different owners to a common lifetime and accepts the call. After the move, the moved operation belongs to the second owner, but handles to it still borrow the first one.

```rust
let first = Block::new(&[], &[]).unwrap();
first.append_operation(OperationBuilder::new("foo.moved", location).build().unwrap());
let handle = first.first_operation().unwrap();

{
    let second = Block::new(&[], &[]).unwrap();
    let anchor = second.append_operation(OperationBuilder::new("foo.anchor", location).build().unwrap());

    first.first_operation_mut().unwrap().move_after(anchor);
}

println!("{}", handle.name().as_string_ref().as_str().unwrap());
```

Dropping `second` frees the moved operation, and printing its name through `handle` segfaults, with or without Guard Malloc. Lifetimes alone can't fix this, because two distinct borrows can have the same lifetime, as two contexts alive in the same scope can. Moves between owners need either the exclusive path below or a generative brand. Until then, the two functions could be marked `unsafe` as #1015 did for the drivers, or check at run time that both operations have the same root.

## How inkwell handles it

Read on inkwell `master` at `04bfd21f4` (2026-11-03). I didn't run these cases.

- Every IR handle, such as `BasicBlock`, `InstructionValue` and `FunctionValue`, is `Copy` and carries only the context lifetime `'ctx`. None is tied to the module or function that owns it.
- Edits take `self` by value or `&self`, for example `set_operand`, `move_after` and `replace_all_uses_with`. Nothing takes `&mut`, so the conflict above never arises.
- Freeing is protected only in part. `BasicBlock::delete`, `FunctionValue::delete` and `GlobalValue::delete` are `unsafe`, and their documentation and TODOs admit that other copies may dangle. `InstructionValue::erase_from_basic_block` is safe.

Its safe interface has these gaps:

| Gap                                                | Where                                                                                                                                                                                                               | Melior counterpart           |
| -------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------- |
| Values outlive their module                        | `Module::add_function(&self)` returns a `FunctionValue<'ctx>`, and dropping the module frees its IR. Issue #343 has been open since 2022, and the maintainer agreed that the lifetime should be tied to the module. | Fixed by #999 and #1000      |
| Erasing an instruction leaves copies of its handle | `InstructionValue::erase_from_basic_block(self)` is safe, with only a review comment about the risk.                                                                                                                | The rewriter's erase methods |
| Erasing a removed instruction dereferences null    | LLVM's `eraseFromParent` dereferences the parent, which is null after `remove_from_basic_block`.                                                                                                                    | Moving detached operations   |
| Iterators fetch the next element in advance        | `InstructionIter` and `BasicBlockIter` read the next element when they yield the current one, so erasing it in the loop body leaves a dangling pointer.                                                             | Block and region iterators   |

Only the first gap has been observed at run time: the program in #343 printed `<badref>` for the maintainer and segfaulted for the reporter.

Melior's stricter model is what makes exclusive accessors hard. inkwell never hits that conflict because it gave up type-level protection of IR structure, and its only remaining tool is marking frees `unsafe`, which it hasn't done for instructions.

## Compile-time designs

Runtime-checked designs, such as `RefCell` around the IR or generational indices into an arena, are out of scope. The three designs below cost nothing at run time.

### 1. Separate editing from freeing

This is the recommended design.

- Shared handles get every edit that frees nothing. `set_operand`, `move_after` within one owner, `set_attribute` and `replace_uses_of_with` move from `OperationMutLike` to the shared side, so holding block arguments, siblings and results while editing compiles.
- Freeing requires exclusive access to the owner. It is reached from `&mut Module` or `&mut Block` through a chain of `&mut self` accessors such as `body_mut`, `first_operation_mut` and `region_mut`. Since nested handles carry the owner's `'a`, an exclusive borrow of the owner ends every shared handle into the tree.
- Because no handle survives into the exclusive section, targets are named through a cursor or a scoped closure, as in `std::collections::linked_list::CursorMut` (unstable) and `Vec::retain`.

```rust
// Edits that free nothing go through shared handles.
let argument = block.argument(0)?;
block.terminator().unwrap().set_operand(0, argument.into())?;

// Freeing needs the owner exclusively, so every shared handle above is dead here.
let mut cursor = module.body_mut().cursor_mut();

while let Some(operation) = cursor.current() {
    if is_dead(operation) {
        cursor.erase_current(); // takes `&mut cursor`, so `operation` is already unusable
    } else {
        cursor.move_next();
    }
}
```

This makes the erasing rewriter methods and the operation-level drivers safe again, since a mutable handle from the exclusive path really is exclusive. It also fixes iteration: only the current element can be erased, through the cursor, so fetching the next one in advance becomes harmless.

Its costs:

- Moves between owners have to go through the exclusive path, or need the brand from design 2, as the previous section explains.
- Rewrite pattern callbacks need their own version. The callback is a closed scope, so handles can borrow the rewriter, and erasing or replacing can take `&mut rewriter` or consume the root. That is a large API change, on top of the rule in #1014 that a pattern which modified the IR counts as matched.
- A shared handle found while reading can't be upgraded into an exclusive one. Erasing becomes two passes: find the targets, then navigate mutably and erase through a cursor or closure.

### 2. Generative brands

An editing scope such as `module.edit(|token, root| ...)` creates a unique, invariant brand lifetime `'id`, as the GhostCell paper and the `ghost-cell`, `qcell` and `generativity` crates do. Handles carry `'id` and a borrow of the token instead of a borrow of the container. Edits take `&token`, and frees take `&mut token`.

Like design 1, this has no runtime cost. The brand also proves that two handles belong to the same tree, which fixes moves between owners and the mixing of contexts that are alive in the same scope, neither of which plain lifetimes can express. The price is a closure-shaped API and an extra lifetime in every signature.

### 3. Deferred freeing

Melior's erase could unlink an operation with `mlirOperationRemoveFromParent` and keep it in a list on the owner, destroying it only when the owner is dropped. By then no handle can be alive, so nothing dangles.

- MLIR's own C++ drivers still free IR internally, so they keep needing exclusive access to the owner.
- Deferred IR still uses live values. The C API has no function to drop all references of an operation, so Melior would have to clear the operands of every nested operation with `mlirOperationSetOperands`.
- Memory is held until the owner is dropped.

## Recommendation

1. Close the move across owners now, either by marking `move_after` and `move_before` `unsafe` or by checking that both operations have the same root.
2. Prototype the cursor of design 1 for erasure. It is self-contained and gives the `unsafe` erasing methods from #1014 a safe counterpart.
3. Move the edits that free nothing to shared handles, then gate the operation-level drivers behind the exclusive path so that they can become safe again.
4. Adopt design 2's brands only if moves between owners or mixed contexts turn out to matter in practice.
