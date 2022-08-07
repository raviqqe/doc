# Nondeterministic parallel computation in Pen

## August 7th, 2022

[@raviqqe](https://github.com/raviqqe)

---

# Overview of Pen

- Functional programming
- Immutable values
- Inductive values
  - Reference counting with in-place mutation
  - No circular dependency
- Capability-based effect system
  - Pass down "effect arguments" to functions.
- Parallel computation without data race
  - Synchronizaton by data structures (e.g. thunks, lazy lists, etc.)

---

# Examples

## Capability-based effect system

```pen
import Os'Console

main = \(os Os) none | error {
  Console'Print(os, "Hello, world!")?

  none
}
```

---

# Examples

## Deterministic parallel computation

```pen
import Os'Console

f = \(x foo, y foo) bar {
  v = go(\() number {
    computeA(x)
  })

  w = computeB(y)

  aggregate(v(), w)
}
```

---

# Examples

## Nondeterministic parallel computation

```pen
import Os'Console

f = \(x foo, y foo) [number] {
  # computeA and computeB produces two series of data computed concurrently.
  race([[number] computeA(x), computeB(y)])
}
```

---

# Nondeterministic parallel computation

- Parallel computation is nondeterministic in general.
- You can't know which piece of computation finishes first (or even if it does!) before running them.
- **Nondeterminism is not necessary** for parallel computation.
  - e.g. purely functional programs can be parallelized automatically.
- **Nondeterminism is sometimes beneficial** in parallel computation.

---

# Common representation of nondeterminisum

## Channels in Go

- a.k.a. concurrent queues

```go
func main() {
  c1 := make(chan string)
  c2 := make(chan string)

  go func() {
    c1 <- "fast"
  }()
  go func() {
    time.Sleep(1 * time.Second)
    c2 <- "slow"
  }()

  select {
  case msg := <-c1:
    fmt.Println(msg)
  case msg := <-c2:
    fmt.Println(msg)
  }
}
```

---

# Functional representation of concurrent queues

---

# Summary

- Progress
  - Reference counting optimization
- Next plans
  - Standard library improvements
  - Application development?
