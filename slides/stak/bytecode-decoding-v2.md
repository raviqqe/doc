# New bytecode decoder for Stak Scheme

[@raviqqe](https://github.com/raviqqe)

March 15, 2025

---

# Contents

- Stak Scheme
- Progress
  - Rust integration
  - New bytecode decoder
- Future work

---

# Stak Scheme

- A bytecode compiler and virtual machine (VM) for Scheme
  - The compiler is written in Scheme.
  - The VM is written in Rust.
- It aims to support the R7RS-small standard.
- Aims to be an interpreter with **small memory footprint**

---

# Progress

- Rust integration
- New bytecode decoder
  - Examples

---

# Rust integration (continued)

- The integration scheme changed a bit since the last meetup.

## Scheme side

- The `(import (stak rust))` statement imports Rust functions passed into a sripting engine.
  - In the following example, it's the `add` function.

```scheme
(import (scheme base) (scheme write) (stak rust))

(display (add 1 2))
```

---

# Rust integration (continued)

## Rust side

```rust
fn main() -> Result<(), Box<dyn Error>> {
    run_scheme(&include_module!("add.scm"))?;

    Ok(())
}

fn run_scheme(module: &UniversalModule) -> Result<(), EngineError> {
    let mut heap = [Default::default(); HEAP_SIZE];
    let mut functions = [("add", r#fn(|x: isize, y: isize| x + y))];

    Engine::new(&mut heap, &mut functions)?.run(module)
}
```

---

# New bytecode decoder

- Stak Scheme has a new bytecode decoder.
- Decodes bytecodes into Markdown.
- Decodes instructions and immediate values.

## Examples

### Hello world program

- Without the library part

```markdown
- constant "Hello, world!\n"
- call 1 #f || ; <- write-string symbol
```

---

## Examples

### Fibonacci function

- Without the library part

```markdown
- constant procedure 1 #f
  - get 0
  - constant 2
  - call 2 #f ||
  - if
    - get 0
  - get 0
  - constant 1
  - call 2 #f ||
  - call 1 #f ||
  - get 1
  - constant 2
  - call 2 #f ||
  - call 1 #f ||
  - call 2 #f ||
- call 1 #f || ; <- $$close primitive function
- set || ; <- fibonacci symbol
```

<style scoped>
    pre {
        font-size: 65%;
    }
</style>

---

# Snapshotting Scheme programs

- The repository now has snapshots of all the decoded bytecodes of Scheme programs.
- I can review all the bytecode changes as diffs on GitHub!

```diff
  - call 1 #f find-quoted-symbols
  - get 5
  - call 1 #f find-quoted-symbols
- - call 4 #f append
+ - get 5
+ - call 1 #f find-quoted-symbols
+ - call 5 #f append
  - call 1 #f unique
  - call 2 #f filter
  - get 4
```

---

# Examples

- [Detect features #2154](https://github.com/raviqqe/stak/pull/2154)
- [Make non-data symbols empty #2168](https://github.com/raviqqe/stak/pull/2168)

---

# Future work

- [Segregate environment for each `eval` call](https://github.com/raviqqe/stak/issues/1997)
- [The "Inception" project](https://github.com/raviqqe/stak/issues/2157)

---

# Summary

- Building Rust integration is fun! 😃
