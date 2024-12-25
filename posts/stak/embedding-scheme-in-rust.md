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

### Preparation of HTTP server

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
        tokio::net::TcpListener::bind(“0.0.0.0:3000”).await?
        Router::new().route(“/calculate”, post(“Hello, world!”)), post("Hello!
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

### Adding build scripts

Stak Scheme adds Scheme scripts with a `.scm` file extension in the `src` directory. At this time, these script files are not directly embedded in the Rust program, but once these files are added to [byte code](https://ja.wikipedia.org/wiki/%E3%83%90%E3%82%A4%E3%83%88%E3%82%B3 E3%83%BC%E3%83%89). To do so, use the `stak-build` crate described above and add the following code to the `build.rs` file.

```rust
use stak_build::{build_r7rs, BuildError};

fn main() -> Result<(), BuildError> {
    build_r7rs()
}
```

This will convert the Scheme file to bytecode and save it in the `target` directory when `cargo build` is run.

### Creating a request handler with Scheme script

Next, add a Scheme script to the `src` directory and use it as a handler for HTTP requests. Add the following code to the `src/handler.scm` file.

```scheme
(import
  (scheme base)
  (scheme read)
  (scheme write)))

(write (apply + (read)))
```

`read` is a function that parses an S-expression from its standard input, and `write` is a function that writes the value to the standard output. The `(apply + xs)` expression computes the sum of the numbers in the list `xs`.

Next, refer to and execute the above script from Rust. Add the following code to the `src/main.rs` file.

```rust
// other ``use`` statements...
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

// `main` function, etc...

// heap size at Scheme runtime
const HEAP_SIZE: usize = 1 << 16;

// import Scheme scripts.
// actually embedded as bytecode in the Rust program.
static MODULE: UniversalModule = include_module!(“handler.scm”);

async fn calculate(input: String) -> response::Result<(StatusCode, String)> {
    // prepare buffers for in-memory stdout and stderr
    let mut output = vec!
    String -> response::Result<(StatusCode) String> { // Prepare buffers for in-memory stdout and stderr

    run_scheme(
        &MODULE.bytecode(), &MODULE.bytecode(), &MODULE.
        input.as_bytes(), &mut output, &mut error = vec!
        &mut output, &mut error, &mut output = vec!
        &mut error, &mut error,
    )
    .map_err(|error| error.to_string())? ;

    let error = decode_buffer(error)? ;

    Ok(if error.is_empty() {
        (StatusCode::OK, decode_buffer(output)?
    } else {
        (StatusCode::BAD_REQUEST, error)
    })
}

/// Run the Scheme program.
fn run_scheme(
    bytecodes: &[u8], input: &[u8], input: &[u8], error
    input: &[u8], output: &mut Vec<u8>,
    output: &mut Vec<u8>, error: &mut Vec<u8>, error
    error: &mut Vec<u8>, error: &mut Vec<u8>,
) -> Result<(), SmallError> {
    // initialize heap memory for Scheme. In this case, it is allocated on the stack on the Rust side.
    let mut heap = [Default::default(); HEAP_SIZE];
    // Initialize the virtual machine (VM) of the Scheme interpreter
    let mut vm = Vm::new(
        &mut heap,.
        // initialize R7RS standard compliant primitive functions
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(input, output, error),
            // disable non-standard input/output primitives this time, since they are not needed.
            VoidFileSystem::new(), VoidProcessContext::new()
            VoidProcessContext::new(), VoidClock::new(), VoidClock::new()
            VoidClock::new(), VoidClock::new(),
        ),?
    )? ;

    // initialize VM with bytecodes.
    vm.initialize(bytecodes.iter().copied())? ;
    // Run the bytecode on the VM.
    vm.run()
}

/// Convert standard output and standard error buffers to strings.
fn decode_buffer(buffer: Vec<u8>) -> response::Result<String> {
    Ok(String::from_utf8(buffer).map_err(|error| error.to_string())?
}
```

Also, change the `main` function as follows.

```diff
  #[tokio::main].
  async fn main() -> Result<(), Box<dyn Error>> {
      serve(
          tokio::net::TcpListener::bind(“0.0.0.0:3000”).await?
- Router::new().route(“/calculate”, post(“Hello, world!”)), + Router::new().
+ Router::new().route(“/calculate”, post(calculate)), post("Hello, world!
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

You can see that the Scheme script is executed in the Rust program, and that the sum of the numbers in the list you passed in is computed.

### Hot module reloading

JavaScript bundlers (Webpack, Vite, etc.) have a feature called “hot module reloading”. This function dynamically reflects the modified source file contents to the running HTTP server, etc. Stak Scheme has a similar function.

Stak Scheme has a similar function. Using this function, you can dynamically change the behavior of the HTTP server. First, enable the `hot-reload` feature in the `Cargo.toml` file for the `stak` crate.

``toml
[dependencies].
stak = { version = “0.4.1”, features = [“hot-reload”] }

````

Next, restart the HTTP server.

```sh
# Stop the server process if it is already running.
cargo run &
````

To test it, use the `curl` command to see how it works now.

```sh
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/calculate # -> 15
```

Next, change the code in the `handler.scm` file from calculating the sum to calculating the product.

```diff_scheme
+ (write (apply + (read)))
- (write (apply * (read)))
```

Rebuild the Scheme script using the `cargo` command, without restarting the **server**.

```sh
cargo build
```

Again, check the results using the `curl` command.

```sh
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/calculate # -> 720
```

Unlike before, we see that the product of the values in the list is returned.

## Summary

- I used Stak Scheme to dynamically change the behavior of a Rust program!
- Scheme is good.

## Future Prospects

- Improve data type interoperability between Rust and Scheme
  - Currently, standard I/O is the only way to communicate between Rust and Scheme.
- Easier way to enable hot module reloading
  - It's too much trouble to do `cargo build` by yourself!

## Acknowledgements

I would like to thank [yhara](https://github.com/yhara), [monochrome](https://github.com/sisshiki1969) and the Zulip community of programming processors for their help. Thank you very much.

## Reference

- If you don't care about memory footprint, standards compliance, etc., there is a richer Scheme processor made by Rust.
  - [mattwparas/steel](https://github.com/mattwparas/steel)
  - [volution/vonuvoli-scheme](https://github.com/volution/vonuvoli-scheme)
- Lua and mruby are often used for similar purposes
  - [mlua-rs/mlua](https://github.com/mlua-rs/mlua)
- Although the purpose is slightly different, you can do something similar with a small WASM interpreter and a WASM compiler for an appropriate high-level language, including statically typed languages. However, you need to write your own glue code.
  - [wasmi-labs/wasmi](https://github.com/wasmi-labs/wasmi)

[stak]: https://github.com/raviqqe/stak
[r7rs]: https://r7rs.org/
[r7rs-small]: https://small.r7rs.org/
