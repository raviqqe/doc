# Stak Scheme: The tiny R7RS-small implementation

## Scheme Workshop 2025

Yota Toyama

---

# Stak Scheme

---

## Background

- Ribbit Scheme is a tiny R4RS-small implementation.
  - The bytecode compiler is written in Scheme.
  - The virtual machine is written in some host language.
- Can we put the whole R7RS-small implementation on its VM? 🤔

---

## Stak Scheme

- Stak Scheme is the tiny R7RS-small implementation.
  - The bytecode compiler is written in Scheme.
  - The virtual machine is written in Rust.
- Can we implement the entire R7RS-small standard on its VM? 🤔

---

###### Stak Scheme

## Virtual machine

---

## Code graphs

---

# Examples

---

## Example: Library system

### Scheme

```scheme
(display "foo")
(display "foo")
(display "bar")
```

---

## Example: Library system

### Code graph

---

# Code graph

![](./fibonacci.svg)
