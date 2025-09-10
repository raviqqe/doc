# Stak Scheme: The tiny R7RS-small implementation

## Scheme Workshop 2025

Yota Toyama

---

# Stak Scheme

---

## Background

- Ribbit Scheme is a tiny R4RS implementation.
  - The bytecode compiler is written in Scheme.
  - The virtual machine is written in some host language.
- Can we implement the entire R7RS-small standard on its VM? 🤔

---

## Stak Scheme

- Stak Scheme is the tiny R7RS-small implementation.
  - The bytecode compiler is written in Scheme.
  - The virtual machine is written in Rust.

---

## Virtual machine

- The stack machine.
- Everything is a pair.
  - Bytecode
  - Values
    - Including lists, characters, strings, etc.

---

## Code graphs

---

# Examples

---

## Duplicate strings

### Scheme

```scheme
(display "foo")
(display "foo")
(display "bar")
```

---

## Duplicate strings

### Code graph

---

## Library system

### Scheme

```scheme
(display "foo")
(display "foo")
(display "bar")
```

---

# Appendix

---

# Fibonacci function

![](./fibonacci.svg)
