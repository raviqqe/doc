# Tree shaking in Stak Scheme

[@raviqqe](https://github.com/raviqqe)

July 20, 2025

---

# Contents

- Stak Scheme
- Progress
- Tree shaking
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
- The compiler is written in Scheme.
- The VM is written in Rust.
- It aims to support R7RS-small.

---

# Progress

- Tree shaking
- Unicode (UTF-8) support in I/O
- The `include` syntax

---

# Tree shaking

- Scheme is an **impure** functional programming language.
- Any procedure calls might have side effects in Scheme.
- Even in libraries, we might have top-level procedure calls.
- Variable definitions might also cause side effects.
- No immutable definitions in the R7RS standard.
- Tree shaking is so difficult. 😃

---

# TypeScript

```typescript
const foo: number = 42;

const bar = (x: number): number => x * foo;

// These results in static analysis or runtime errors.
foo = 0;
bar = (x) => x;
```

---

# Scheme

```scheme
(define foo 42)

(define (bar x)
  (* x foo))

; Any top-level definitions might be overwritten at runtime.
(set! foo 0)
(set! bar #f)
```

---

# Future work

- Debug mode
  - Stack trace
- Unicode in the `(scheme char)` library
