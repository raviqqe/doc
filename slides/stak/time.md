# Progress in Stak Scheme

October 13, 2024

--

# Contents

- Startup time optimization

--

# Startup time optimization

- Now, the Scheme interpreter written by Stak Scheme itself starts in ~200 ms on macOS!
- It's basically REPL without the print part.

--

# `(scheme time)` library

- Time procedures
  - `current-jiffy`
  - `current-second`
  - `jiffies-per-second`
- Implemented by Rust's `std` or `libc`

--

# `(scheme time)` library

- Time procedures
  - `current-jiffy`
  - `current-second`
  - `jiffies-per-second`
- Implemented by Rust's `std` or `libc`

--

# Others

- Adopting `core::error`
