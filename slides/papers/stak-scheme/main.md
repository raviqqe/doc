# Stak Scheme: The tiny R7RS-small implementation

## Scheme Workshop 2025

Yota Toyama

---

# Stak Scheme

---

## Background

- Ribbit Scheme is a tiny R4RS-small implementation in 7 KB!
  - The bytecode compiler is written in Scheme.
  - The virtual machine is written in some host language.
- Can we put the whole R7RS-small implementation on its VM? 🤔

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
