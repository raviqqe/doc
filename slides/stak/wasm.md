# Stak Scheme on WASM

[@raviqqe](https://github.com/raviqqe)

---

# Overview

- Shibuya.lisp #114
- Rust on WASM
- Stak Scheme on WASM
- Demo

---

# I went to Shibuya.lisp!

- [Shibuya.lisp Lisp meetup #114](https://lisp.connpass.com/event/313757/)
- A meetup to talk about Lisp
- Common Lisp, Clojure, Scheme, etc.
- I talked about the overview of Stak Scheme and how it works.

---

# Rust on WASM

- [wasm-pack](https://github.com/rustwasm/wasm-pack)
  - A command to deploy a npm package in WASM
- [wasm-bindgen](https://github.com/rustwasm/wasm-bindgen)
  - A crate to generate bindings from Rust in WASM to JavaScript (TypeScript)

---

# Stak on WASM

- [`@raviqqe/stak`](https://www.npmjs.com/package/@raviqqe/stak)
- Exposes a few functions.
  - `compile`: Compiles Scheme source codes into VM bytecodes.
  - `interpret`: Interprets VM bytecodes.
  - `decode`: Decodes bytecodes into their text format.

---

# Demo

<iframe src="https://raviqqe.com/stak/demo" style="width: 100%; height: 100%; border: none;"></iframe>
