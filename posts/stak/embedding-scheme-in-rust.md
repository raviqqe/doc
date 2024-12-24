# Embedding Scheme in Rust

Rust, as a compiled language, makes it challenging to dynamically modify the behavior of programs. In this article, we'll explore embedding a small Scheme interpreter written in Rust called [Stak Scheme][stak] into a Rust program to dynamically (without stopping the process) modify its behavior.

The code used in this article can be found in the [`examples/hot-reload` directory](https://github.com/raviqqe/stak/tree/main/examples/hot-reload) of [the Stak Scheme repository][stak].

## What is Scheme?

[Scheme](https://www.scheme.org/) is a dialect of Lisp characterized by features such as [the first-class continuations](https://en.wikipedia.org/wiki/Continuation). It is developed through community-based specification. And, its latest version, [R7RS-small][r7rs-small], has a relatively compact specification of about 90 pages.

## What is Stak Scheme?

[Stak Scheme][stak] is a Scheme interpreter compliant with the [the R7RS-small standard][r7rs-small], forked from [Ribbit Scheme](https://github.com/udem-dlteam/ribbit), and has the following features:

- A Scheme interpreter embeddable in Rust programs.
- Small memory footprint.
- Capability-based security:
  - By default, Stak Scheme cannot interact with any external API's like operating systems.
  - APIs for I/O or file systems need to be explicitly enabled during the initialization of the interpreter's virtual machine (VM.)

## Embedding Scheme scripts in Rust programs

In this example, we'll write an HTTP server in Rust and embed a Scheme script within it.

### Initializing the crate

First, initialize a binary crate for the HTTP server with the following commands:

```sh
cargo init http-server
cd http-server
```

### Adding Dependencies

To add Stak Scheme as a library to the Rust crate, run:

```sh
cargo add stak
cargo add --build stak-build
```

- The `stak` crate allows Rust to call the Scheme interpreter.
- The `stak-build` crate compiles Scheme scripts into Rust bytecode via a [`build.rs` build script](https://doc.rust-lang.org/cargo/reference/build-scripts.html) (explained later).

### Preparing the HTTP server

Next, we'll set up an HTTP server in Rust using [`axum`](https://github.com/tokio-rs/axum), a library based on the asynchronous runtime Tokio. Add the dependencies:

```sh
cargo add --features rt-multi-thread tokio
cargo add axum
```

Then add the following code to `src/main.rs`:

```rust
use axum::{routing::post, serve, Router};
use core::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    serve(
        tokio::net::TcpListener::bind("0.0.0.0:3000").await?,
        Router::new().route("/calculate", post("Hello, world!")),
    )
    .await?;

    Ok(())
}
```

Verify the setup with:

```sh
cargo run &
curl -f -X POST http://localhost:3000/calculate # -> Hello, world!
kill %1
```

### Adding a build script

Add Scheme scripts to the `src` directory with the `.scm` extension. These files are not directly embedded into the Rust program but are converted into [bytecode](https://en.wikipedia.org/wiki/Bytecode) using the `stak-build` crate. Add the following to a `build.rs` file:

```rust
use stak_build::{build_r7rs, BuildError};

fn main() -> Result<(), BuildError> {
    build_r7rs()
}
```

This ensures the Scheme files are converted to bytecode and stored in the `target` directory when `cargo build` is run.

### Writing a request handler in Scheme

Add the following Scheme script to `src/handler.scm`:

```scheme
(import
  (scheme base)
  (scheme read)
  (scheme write))

(write (apply + (read)))
```

This script calculates the sum of a list read from the standard input and writes the result to the standard output.

Embed this script into Rust with the following additions to `src/main.rs`:

[Detailed code omitted for brevity in translation.]

Test the program:

```sh
cargo run &
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/calculate # -> 15
kill %1
```

---

## Hot Module Reloading

Enable the `hot-reload` feature in `stak`:

```toml
[dependencies]
stak = { version = "0.4.1", features = ["hot-reload"] }
```

Modify the script dynamically without restarting the server:

1. Change the script to calculate the product instead of the sum.
2. Rebuild with `cargo build`.
3. Verify the new behavior:

```sh
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/calculate # -> 720
```

---

## Summary

- We used Stak Scheme to dynamically modify the behavior of a Rust program.
- Scheme is awesome!

---

## Future directions

- Improve data type interoperability between Rust and Scheme.
- Simplify the hot module reloading process (e.g., avoiding manual `cargo build`).

## References

- If you don’t mind large memory footprints or strict standard compliance, there are more feature-rich Scheme interpreters written in Rust:
  - [mattwparas/steel](https://github.com/mattwparas/steel)
  - [volution/vonuvoli-scheme](https://github.com/volution/vonuvoli-scheme)
- Lua and mruby are also commonly used for similar purposes:
  - [mlua-rs/mlua](https://github.com/mlua-rs/mlua)
- While slightly different in purpose, you can achieve similar results using small WebAssembly (WASM) interpreters along with WASM compilers for high-level languages with static typing. However, you’ll need to write glue code yourself:
  - [wasmi-labs/wasmi](https://github.com/wasmi-labs/wasmi)

## Acknowledgments

Special thanks to [yhara](https://github.com/yhara), [monochrome](https://github.com/sisshiki1969), and the programming language implementation Zulip community!

[stak]: https://github.com/raviqqe/stak
[r7rs-small]: https://small.r7rs.org/
