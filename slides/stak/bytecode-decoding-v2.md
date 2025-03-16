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

## Scheme side

`(import (stak rust))` imports Rust functions passed into a sripting engine.

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
