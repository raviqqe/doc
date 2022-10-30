# Compiler optimization

---

# Pen's compiler is 4.5 times faster!

- Since the last meetup

---

## What made the compiler faster?

- Code
  - Imperative vs. functional
- Data
  - Minimizing data
  - Sharing data
  - "Compressing" IR

---

## Imperative algorithms

---

## "Compressing" IR

- It's better to "decompress" IR (intermediate representation) later.
  - Function inlining
- All the later passes get slower by the increased data size.
- LLVM handles the "decompression" anyway.

---

## Others

- foo

---

# Results

---

# Future work

- Function passes

---

# Conclusion

- Small data makes (apparently)
