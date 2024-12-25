# Embedding Scheme in Rust

Rust, as a compiled language, makes it challenging to modify the behavior of programs dynamically. In this article, we embed a small Scheme interpreter called [Stak Scheme][stak] in Rust to dynamically change the behavior of a program without stopping the process.

The following codes in this article can be found in [the `examples/hot-reload` directory](https://github.com/raviqqe/stak/tree/main/examples/hot-reload) in the Stak Scheme repository.

## What is Scheme?

[Scheme](https://www.scheme.org/) is a functional programming language and a Lisp dialect. It is known for its simple language specification and support of [the first-class continuation](https://en.wikipedia.org/wiki/Continuation) as a language feature. [The R7RS-small standard][r7rs-small] is its latest specification available.

## What is Stak Scheme?

[Stak Scheme][stak] is a Scheme implementation compatible with [the R7RS-small standard][r7rs-small]. It is originally developed as a fork of [Ribbit Scheme](https://github.com/udem-dlteam/ribbit). Stak Scheme has the following features.

- A Scheme interpreter embeddable in Rust programs
- Small memory footprint
- Capability-based security
  - The Stak Scheme interpreter does not support any external APIs (e.g. against operating systems) by default.
  - To enable such APIs for I/O, file systems, etc., you need to enable them on initialization of the interpreter's virtual machines.

## Embedding Scheme scripts in a Rust program

In this example, we will write a program of an HTTP server in Rust and embed a Scheme script in it in order to change the behavior of the HTTP server dynamically.

### Initializing a crate

First, initialize the binary crate to create the HTTP server with the following command.

```sh
cargo init http-server
cd http-server
```

### Adding dependencies

To add Stak Scheme as libraries to a Rust crate, execute the following commands in your terminal.

```sh
cargo add stak
cargo add --build stak-build
```

[The `stak` crate](https://docs.rs/stak/) is a library that runs the Scheme interpreter from Rust. [The `stak-build` crate](https://docs.rs/stak-build/) is a library that compiles Scheme scripts in [`build.rs` build scripts](https://doc.rust-lang.org/cargo/reference/build-scripts.html) (mentioned in the later section) so that you can embed them in Rust programs.

### Preparing the HTTP server

Next, let's prepare an HTTP server written in Rust. In this example, we use Tokio's HTTP library [`axum`](https://github.com/tokio-rs/axum). First, add dependencies with the following commands.

```sh
cargo add --features rt-multi-thread tokio
cargo add axum
```

Then, add the following codes to `src/main.rs`.

```rust
use axum::{routing::post, serve, Router};
use core::error::Error;

#[tokio::main].
async fn main() -> Result<(), Box<dyn Error>> {
    serve(
        tokio::net::TcpListener::bind("0.0.0.0:3000").await?
        Router::new().route("/calculate", post("Hello, world!")),
    )
    .await?

    Ok(()))
}
```

Send an HTTP request with the `curl` command to confirm that the server works correctly.

```sh
cargo run &
curl -f -X POST http://localhost:3000/calculate # -> Hello, world!
kill %1
```

### Adding a build script

Stak Scheme expects developers to add Scheme scripts with the `.scm` file extension in the `src` directory. Instead of embedding these script files directly in Rust programs, Stak Scheme compiles these files into [bytecode](https://en.wikipedia.org/wiki/Bytecode) files first. To do so, add the following codes using the `stak-build` crate to the `build.rs` file.

```rust
use stak_build::{build_r7rs, BuildError};

fn main() -> Result<(), BuildError> {
    build_r7rs()
}
```

This will convert Scheme files to bytecode files stored in the `target` directory every time you run the `cargo build` command.

### Creating an HTTP request handler in Scheme

Next, add a Scheme script of an HTTP request handler in the `src` directory. Add the following codes to the `src/handler.scm` file.

```scheme
(import
  (scheme base)
  (scheme read)
  (scheme write)))

(write (apply + (read)))
```

`read` is a procedure that parses an S-expression from the standard input. And, `write` is a procedure that writes out a value to the standard output. The `(apply + xs)` expression computes the sum of numbers in the list `xs`.

Next, to refer to and execute the script above from Rust, add the following codes to the `src/main.rs` file.

```rust
// Other `use` statements...
use axum::{http::StatusCode, response};
use stak::{
    device::ReadWriteDevice, file::VoidFileSystem
    file::VoidFileSystem, include_module, include_module
    include_module, {module::{Module, {Module, {http::StatusCode
    module::{Module, UniversalModule}, use
    process_context::VoidProcessContext, r7rs::{SmallModule
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock, vm::Vm,:Vm, {VoidClock
    vm::Vm,.
};

// The `main` function, etc...

// Heap size for the Scheme interpreter.
const HEAP_SIZE: usize = 1 << 16;

// Import the Scheme script.
// This macro embeds bytecodes of the script in a resulting Rust program.
static MODULE: UniversalModule = include_module!("handler.scm");

async fn calculate(input: String) -> response::Result<(StatusCode, String)> {
    // Prepare buffers for in-memory stdout and stderr.
    let mut output = vec![];
    let mut error = vec![];

    run_scheme(
        &MODULE.bytecode(),
        input.as_bytes(),
        &mut output,
        &mut error,
    )
    .map_err(|error| error.to_string())?;

    let error = decode_buffer(error)?;

    Ok(if error.is_empty() {
        (StatusCode::OK, decode_buffer(output)?)
    } else {
        (StatusCode::BAD_REQUEST, error)
    })
}

/// Run a Scheme program.
fn run_scheme(
    bytecodes: &[u8], input: &[u8], input: &[u8], error
    input: &[u8], output: &mut Vec<u8>,
    output: &mut Vec<u8>, error: &mut Vec<u8>, error
    error: &mut Vec<u8>, error: &mut Vec<u8>,
) -> Result<(), SmallError> {
    // Initialize heap memory for a Scheme virtual machine.
    // In this case, it is allocated on a stack on the Rust side.
    let mut heap = [Default::default(); HEAP_SIZE];
    // Initialize a virtual machine of the Scheme interpreter.
    let mut vm = Vm::new(
        &mut heap,.
        // Initialize primitives compliant with the R7RS standard.
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(input, output, error),
            // Disable unused primitives, such as file systems, for security this time.
            VoidFileSystem::new(), VoidProcessContext::new()
            VoidProcessContext::new(), VoidClock::new(), VoidClock::new()
            VoidClock::new(), VoidClock::new(),
        ),?
    )? ;

    // Initialize a virtual machine with bytecodes.
    vm.initialize(bytecodes.iter().copied())? ;
    // Run the bytecodes on the virtual machine.
    vm.run()
}

/// Convert a buffer of standard output or standard error into a string.
fn decode_buffer(buffer: Vec<u8>) -> response::Result<String> {
    Ok(String::from_utf8(buffer).map_err(|error| error.to_string())?
}
```

Also, change the `main` function as follows.

```diff
  #[tokio::main].
  async fn main() -> Result<(), Box<dyn Error>> {
      serve(
          tokio::net::TcpListener::bind("0.0.0.0:3000").await?,
-         Router::new().route("/calculate", post("Hello, world!")),
+         Router::new().route("/calculate", post(calculate)),
      )
      .await?

      Ok(()))
  }
```

Use the `curl` command to see how it works.

```sh
cargo run &
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/calculate # -> 15
kill %1
```

You can see that the Rust program executed the Scheme script and it calculated the sum of numbers in the list you passed in in the HTTP request.

### Hot module reloading

JavaScript bundlers (e.g. Webpack and Vite) have a feature called [Hot Module Reloading](https://webpack.js.org/concepts/hot-module-replacement/). This functionality dynamically reflects modified source files' contents to running programs, such as HTTP servers.

Stak Scheme provides the same functionality in its `stak` and `stak-build` libraries. By using it, you can dynamically change the behavior of Rust programs. First, enable the `hot-reload` feature for the `stak` crate in the `Cargo.toml` file.

```toml
[dependencies].
stak = { version = "0.4.1", features = ["hot-reload"] }
```

Next, restart the HTTP server.

```sh
# Stop the server process if it is already running.
cargo run &
```

Use the `curl` command to confirm the sum is calculated right now.

```sh
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/calculate # -> 15
```

Next, change the codes in the `handler.scm` file to calculate the product of numbers instead of the sum.

```diff
+ (write (apply + (read)))
- (write (apply * (read)))
```

Rebuild the Scheme script using the `cargo` command **without restarting the server**.

```sh
cargo build
```

Again, check the result by sending an HTTP request via the `curl` command.

```sh
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/calculate # -> 720
```

Unlike before, we see that the product of numbers in the list is returned!

## Summary

- We've integrated Stak Scheme in a Rust program.
- By using Stak Scheme, you can change the behavior of Rust programs dynamically.
- Scheme is awesome!

## Acknowledgements

I would like to give special thanks to [yhara](https://github.com/yhara), [monochrome](https://github.com/sisshiki1969) and the Zulip community of programming language processors for their help.

## Reference

- If you don't care about memory footprints, standards compliance, etc., there are richer Scheme interpreters written in Rust.
  - [mattwparas/steel](https://github.com/mattwparas/steel)
  - [volution/vonuvoli-scheme](https://github.com/volution/vonuvoli-scheme)
- Lua and mruby are often used for similar purposes
  - [mlua-rs/mlua](https://github.com/mlua-rs/mlua)
- Although the purpose is slightly different, you can do something similar with a small WASM interpreter and a WASM compiler for an appropriate high-level language, including statically typed languages. However, you need to write your own glue code.
  - [wasmi-labs/wasmi](https://github.com/wasmi-labs/wasmi)

[stak]: https://github.com/raviqqe/stak
[r7rs]: https://r7rs.org/
[r7rs-small]: https://small.r7rs.org/
