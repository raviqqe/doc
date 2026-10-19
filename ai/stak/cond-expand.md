---
description: An AI design study of how Stak Scheme can support `cond-expand` in library declarations, including moving `include` into the library and macro expansion stages and carrying the originating directory between them.
---

# `cond-expand` in library declarations in Stak Scheme

- Date: 2026-10-18
- Model: Claude Opus 5.5
- Repository: [`raviqqe/stak`](https://github.com/raviqqe/stak)
- Version: `main` at `1243edc02`

## Summary

- A `cond-expand` library declaration is silently dropped today. `register-library!` keeps only `import`, `export`, and `begin` declarations, and both compilers register libraries before macro expansion, where the `cond-expand` macro lives. The compiler also has no feature list; only the prelude's `define-features` macro has one.
- The recommended design splices `cond-expand` declarations in `register-library!` and moves the feature list from the prelude into the compiler. One requirement evaluator there serves both library declarations and a generalized `$$if-library` special form behind the expression-level macro.
- R7RS has two different `include` forms. The expression-level one is syntax and belongs in macro expansion, while the library declaration one, together with `include-ci` and `include-library-declarations`, belongs in the library stage. Today one syntactic pass handles both before either stage, so it reads files in unselected `cond-expand` branches and inside quoted data, regardless of bindings.
- Moving the expression-level `include` into macro expansion needs the directory of the code being expanded. A `directory` field on the macro context, set by a `$$directory` marker form that `expand-library-bodies` wraps around each library body, carries it across the stage boundary. Nested includes need no marker because the expander knows each included file's path when it reads the file.

## Current behavior (verified)

- Library declarations: `register-library!` (around line 445 of `compile.scm`) collects declarations with `collect-bodies`, which filters the declarations of a `define-library` form by their head symbols. A `cond-expand` declaration matches none of them and disappears together with the imports inside it. The program below fails with `procedure expected` because the library body never sees `(scheme base)`.

  ```scheme
  (define-library (foo)
    (export foo)

    (cond-expand
      (stak
        (import (scheme base)))
      (else
        (import (scheme base) (scheme write))))

    (begin
      (define (foo) (write-u8 65))))

  (import (foo))

  (foo)
  ```

- Stage order: the batch compiler runs `include-files`, `expand-libraries`, and `expand-macros` in this order in `compile-program`. The runtime compiler that `incept` generates as `(stak compile)` runs the same steps for each expression. `cond-expand` is a `syntax-rules` macro exported by `(scheme base)`, so it cannot run while library declarations are read. In the batch compiler, no macro exists at that point because macros are defined while library bodies are expanded.
- Feature list: the only list is in the `define-features` macro around line 333 of `prelude.scm`, which defines both `(features)` and `cond-expand`. Feature identifiers are matched as `syntax-rules` literals, so `full-unicode`, `ieee-float`, and `little-endian`, which `(scheme base)` does not export as identifiers, never match in user code even though `(features)` lists them. The compiler knows only the `$$if-library` special form (around line 847 of `compile.scm`), which the macro uses for `(library ...)` requirements. It checks registered libraries and `.sld` files on the load path with `library-exists?`.
- Inclusion: `include-files` runs before both stages and rewrites `include` and `include-library-declarations` anywhere in the source tree, including unselected `cond-expand` branches. The program below fails with `cannot open file`. `include` is not bound anywhere, as `prelude.scm` never mentions it, and `include-ci` is not implemented.

  ```scheme
  (import (scheme base))

  (cond-expand
    (foo
      (include "missing.scm"))
    (else
      (write-u8 65)))
  ```

## Where to evaluate the clauses

### Option A: splice them in `register-library!` (recommended)

Before `collect-bodies` runs, flatten the declarations recursively and replace each `cond-expand` declaration with the declarations of its first fulfilled clause.

- `(library ...)` requirements use `library-exists?`, which sees the prelude's libraries, libraries defined earlier in the program, and `.sld` files on the load path. This matches the expression-level behavior.
- The batch compiler, the runtime compiler, and `.sld` loading share `register-library!` through the compiler frontend, so all of them get the feature.
- The change is local to the library system.
- `include-files` still reads files in unselected branches, as it does for expression-level `cond-expand` today.

### Option B: evaluate them in `include-files`

Dropping unselected branches before inclusion avoids reading their files. However, no library is registered yet when `include-files` runs, so `(library ...)` requirements cannot see the prelude's libraries or libraries defined in the program.

### Option C: process all library declarations in the library stage

This is Option A plus processing the `include`, `include-ci`, and `include-library-declarations` declarations one at a time while registering a library, with `include-files` called only on the selected `begin` bodies. It follows R7RS most closely and never reads files in unselected branches, but it is the largest change. It matters when a branch for another implementation includes a file that does not exist.

### Rejected: reusing the `cond-expand` macro

Library registration precedes macro expansion in both compilers, and the batch compiler has no macros at that point.

## Where the feature list lives

### Option a: copy the list into the compiler

This is the least work, but the two copies of the list can drift apart.

### Option b: move the list into the compiler (recommended)

Generalize `$$if-library` into a special form that evaluates whole feature requirements (`and`, `or`, `not`, `library`, and feature identifiers), and let Option A's splicing call the same evaluator. The prelude macro then shrinks to iterating over clauses.

```scheme
(define-syntax cond-expand
  (syntax-rules (else)
    ((_)
      (syntax-error "unfulfilled cond-expand"))
    ((_ (else body ...))
      (relaxed-begin body ...))
    ((_ (requirement body ...) clause ...)
      ($$if-requirement
        requirement
        (relaxed-begin body ...)
        (cond-expand clause ...)))))
```

`(features)` would get the list from another small special form. Because requirements are evaluated as data rather than matched as literals, `full-unicode`, `ieee-float`, and `little-endian` start matching. Requirement symbols need de-renaming before comparison, as `$$if-library` already does for library names.

### Option c: keep the list in the prelude and let the compiler read it

The runtime compiler would also need the list shipped as metadata, as `$$libraries` is. This has more moving parts than Option b for no benefit.

## The two `include` forms

- The expression-level `include` and `include-ci` are syntax (R7RS section 4.1.7) exported by `(scheme base)`, so they belong in macro expansion. There, `include` no longer fires in unselected `cond-expand` branches, in quoted data or `case` datums, or where a program binds its own `include`.
- The `include`, `include-ci`, and `include-library-declarations` library declarations (R7RS section 5.6.1) belong in the library stage. `include-library-declarations` has to be there because it can bring in `import` and `export` declarations, which are needed before macro expansion. A declaration-level `include` becomes a `begin` body, which an existing scenario in `features/library.feature` relies on.

Moving the expression-level form changes some behavior:

- `(scheme base)` has to export `include`, so `include` works only after importing it.
- A file included at a program's top level can no longer contain `import` or `define-library` forms because the library stage runs first. R7RS does not allow this, and no scenario relies on it.

Once both forms move, `include-files` disappears. The two moves can land separately. The library stage comes first because declaration-level `cond-expand` needs it, and `include-files` skips `define-library` forms in the meantime so that it does not read files in unselected declaration branches.

## Carrying the originating directory

Nested includes resolve relative to the including file, and includes in a library body resolve relative to its `.sld` file. The macro expander has no source locations, and `expand-library-bodies` splices library bodies into the program, which loses where they came from. Therefore, a directory has to cross the boundary between the two stages.

### Library stage

- `load-library` already passes `(path-directory path)` to `include-files`, and it would pass the directory to `register-library!` instead. Libraries defined in the program get `""`, the current directory, as they do today.
- A new `directory` field on the `library` record stores the directory. The runtime compiler's table of built-in libraries passes `""`.
- `expand-library-bodies` wraps each body as `($$directory "<directory>" body ...)`.

### Macro expansion stage

- `macro-context` gets a `directory` field next to `state` and `locals`. `macro-context-append` copies it, and the initial contexts in `expand-macros` and the runtime compiler start with `""`.
- `expand-macro` gets two cases next to `$$if-library`.
  - `$$directory` expands its body in a context with the directory replaced and returns `($$begin ...)`. This is safe at the top level because `$$define` always defines a global.
  - `$$include` resolves each path against the context's directory and expands each file's forms in a context whose directory is the file's own. This handles nested includes without markers.
- `(scheme base)` exports the user-facing form.

  ```scheme
  (define-syntax include
    (syntax-rules ()
      ((_ path ...)
        ($$include path ...))))
  ```

The marker survives the library stage because symbols prefixed with `$$` are built-in symbols that library renaming leaves alone. It disappears in macro expansion, so later passes never see it. Both compilers share the code, so `eval` supports it too.

### Decisions this design forces

- Use site or definition site: a `syntax-rules` transformer returns the use-site context with its renames appended, so an `include` produced by a macro resolves relative to where the macro is used. To resolve it relative to where the macro is defined, `make-transformer` would capture the directory of its own context and put it into the context it returns. R7RS leaves this open.
- The program's own directory stays `""`. The driver could pass the script's directory instead, which would make top-level includes stop resolving against the current directory, but that is a separate behavior change.

### Rejected alternatives

- Handing `(directory . forms)` chunks from the library stage to macro expansion and expanding each chunk with its directory needs no marker. However, it changes the interface between `expand-libraries` and `expand-macros` in both compilers, and the runtime compiler's import case returns a single `$$begin` expression.
- Syntax objects with source locations are the general solution and would also give file names and line numbers in errors, but the expander works on plain S-expressions throughout.
- A side table keyed by list cells with `eq?` breaks because `resolve-environment-symbols` rebuilds every list in library bodies.
- Making include paths absolute in the library stage requires finding `include` forms syntactically, which is the problem being removed.

## Recommendation

1. Splice `cond-expand` library declarations in `register-library!` (Option A) with a requirement evaluator in the compiler frontend, and move the feature list there (Option b), generalizing `$$if-library` for the expression-level macro.
2. Process the `include`, `include-ci`, and `include-library-declarations` declarations in the library stage as well (Option C), with `include-files` skipping `define-library` forms. This can wait until a library hits a missing file in an unselected branch.
3. Move the expression-level `include` into macro expansion with a `directory` field on the macro context and a `$$directory` marker around library bodies, which removes `include-files`.

Every step changes the compiler frontend. The bytecode snapshots of the programs that embed the compiler for `eval` (`run.scm`, `repl.scm`, `compile.scm`, and the `eval` benchmark) change, and the frontend has to stay portable R7RS for the CI job that compares bytecode compiled by Chibi Scheme, Guile, and Stak byte for byte.

## Decisions only the maintainer can make

- Whether an `include` inside a macro template resolves relative to the macro's use site or its definition site.
- Whether top-level program includes should resolve relative to the script's directory.
- Whether to support `cond-expand` around `import` declarations at a program's top level. R7RS does not require it. Chibi Scheme 0.12.0 and Guile 3.0.11 accept it, while Gauche 0.9.15 does not because `cond-expand` is unbound there before the first import. The same splicing could apply to the top-level forms in `expand-libraries`.

## Sources

- [R7RS small](https://small.r7rs.org/attachment/r7rs.pdf), sections 4.1.7 (inclusion), 4.2.1 (`cond-expand`), and 5.6.1 (library syntax)
- [stak#4141](https://github.com/raviqqe/stak/pull/4141), which added `$$if-library`
- [stak#4144](https://github.com/raviqqe/stak/pull/4144), which added `include-library-declarations`
