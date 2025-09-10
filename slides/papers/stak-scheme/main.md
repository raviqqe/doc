# Stak Scheme: The tiny R7RS-small implementation

## Scheme Workshop 2025

Yota Toyama

---

# Stak Scheme

---

# Examples

```scheme
(import (scheme base) (scheme write))

(define (print-value x)
  (write x)
  (newline))

(display "Hello, ")
(display-value 42)

(display "Hello, ")
(display-value 'foo)
```

---

# Portable memory snapshot

![](./code-graph.svg)
