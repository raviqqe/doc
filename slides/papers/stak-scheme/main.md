# Stak Scheme: The tiny R7RS-small implementation

## Scheme Workshop 2025

Yota Toyama

---

# Stak Scheme

---

# Examples

```scheme
(import (scheme base) (scheme write))

(define (display-name x)
  (display x)
  (newline))

(display "Hello, ")
(display-name 42)

(display "Hello, ")
(display-name 'foo)
```

---

# Portable memory snapshot

![](./fibonacci.svg)
