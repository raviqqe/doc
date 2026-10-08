---
description: An AI survey of default library directories across Scheme implementations and what Stak Scheme needs to support the snow-chibi package manager.
---

# Default library directories in Stak Scheme

<!-- cspell: ignore akku impls loko meevax sitelib sitelibdir tklos userlib ypsilon -->

- Date: 2026-10-01
- Updated: 2026-10-08
- Model: Claude Opus 5.5
- Repository: [`raviqqe/stak`](https://github.com/raviqqe/stak)
- Version: `main` at `32410b5a4`
- Issue: [#4100](https://github.com/raviqqe/stak/issues/4100)

## Summary

- The issue asks Stak to search a default library directory and to report it, so that snow-chibi can install packages from snow-fort for Stak with `snow-chibi install --impls=stak "(srfi 180)"`.
- Stak already has the SRFI 138 `-I` and `-A` flags and ignores directories that do not exist, so the only missing piece for the issue itself is a default directory list and a way to print it.
- Meevax went through the same request from the same requester in 2025 and 2026. It added `meevax --library-directories`, which prints one directory per line, with `$XDG_DATA_HOME/meevax` first and `$PREFIX/share/meevax` second. snow-chibi reads the first line.
- Three other gaps affected snow-chibi at that version: `(exit 0)` exits with status 1, `cond-expand` checks `(library ...)` against a fixed list of six libraries, and `include-library-declarations` is unsupported. All three have been fixed on `main` since, but the `cond-expand` fix makes snow-chibi take the partial built-in `(srfi 1)` for a complete one.

## What snow-chibi needs from an implementation

These facts come from the upstream source in `lib/chibi/snow/`.

- `known-implementations` in `utils.scm` lists each implementation's binary name, version command, and a command that prints its feature list. Most of these commands evaluate an expression with a flag such as `-e`, while the ones for Loko and Mosh write a temporary program file.
- `get-install-dirs` in `commands.scm` returns the implementation's library directories, and snow-chibi installs packages into the first one. It accepts an unknown implementation after a confirmation prompt and falls back to `/usr/local/share/snow/<name>`, or `<prefix>/share/snow/<name>` with `--install-prefix`.
- `scheme-program-command` runs probes and package tests as `<impl> -A <install-dir> [-A <package-dir>] file.scm`. For an unknown implementation, it returns no command, so tests are skipped.
- The install directory may not exist yet when snow-chibi runs its first probe, so `-A` with a missing directory must not fail. This is what [meevax#505](https://github.com/yamacir-kit/meevax/issues/505) fixed.
- The native SRFI probe is a generated program with `(cond-expand ((or (library (srfi N)) srfi-N) N) (else #f))` for every N below 500, ending with `(exit 0)`. snow-chibi reads its output and ignores its exit status.
- The test runner runs each package's own test program and treats it as failed if the exit status is nonzero or the output contains `FAIL` or `ERROR`. `(test-exit)` in `(chibi test)` exits with a boolean.
- Installed programs get a `#! /path/to/<impl>` shebang line.
- The version parser takes the second word of the first output line when the first word is the implementation name, so `stak --version` printing `stak 0.12.28` already works.

## Stak at `32410b5a4` (verified)

| Requirement                               | Status                                                                                                    |
| ----------------------------------------- | --------------------------------------------------------------------------------------------------------- |
| `-A`/`-I` flags                           | Supported by the interpreter and `stak-compile`                                                           |
| `-A` with a missing directory             | Works                                                                                                     |
| `.sld` file with `include` relative to it | Works                                                                                                     |
| Shebang scripts                           | Work                                                                                                      |
| `stak --version`                          | Prints `stak 0.12.28`                                                                                     |
| Default library directories               | None; the `library-paths` parameter starts empty                                                          |
| A way to print library directories        | None from the command line; `(library-paths)` in `(stak compile)`                                         |
| `(exit 0)`                                | Exits with status 1 and prints `Error: halt`; fixed by [#4143](https://github.com/raviqqe/stak/pull/4143) |
| `cond-expand` with `(library (srfi 1))`   | False, although `(srfi 1)` is built in; fixed by [#4141](https://github.com/raviqqe/stak/pull/4141)       |
| `include-library-declarations`            | Fails with `procedure expected`; fixed by [#4144](https://github.com/raviqqe/stak/pull/4144)              |
| `-e` flag to evaluate an expression       | None; the feature probe must go through a temporary file                                                  |

The `stak` binary is defined by `stak_sac::main!` in `sac/src/lib.rs`, which parses arguments with clap before the Scheme program runs. `-V` and `--version` print the version and exit there. Every other argument, including `-s` and `--heap-size`, which clap also reads, reaches `run.scm` through the raw command line, so `stak -s 2000000 foo.scm` fails with `unknown option "-s"`. A new flag therefore belongs in `run.scm`.

The `mstak` binary, a `no_std` build, runs the same `run.scm`, but its process context has no environment variables, so `get-environment-variable` always returns `#f` there.

## Other implementations

The paths below are from Homebrew installations probed locally, except for Meevax, which comes from its source.

| Implementation | Default directories                                                                     | Environment variable        | How to query                                           |
| -------------- | --------------------------------------------------------------------------------------- | --------------------------- | ------------------------------------------------------ |
| Chibi 0.12.0   | `$PREFIX/share/chibi`, `$PREFIX/lib/chibi`, `/usr/local/{share,lib}/snow`, `./lib`, `.` | `CHIBI_MODULE_PATH`         | `(current-module-path)`                                |
| Gauche 0.9.15  | `$PREFIX/share/gauche-0.98/site/lib`, then the versioned system library                 | `GAUCHE_LOAD_PATH`          | `gauche-config --sitelibdir`, `*load-path*`, `gosh -V` |
| Guile 3.0.11   | `$PREFIX/share/guile/3.0`, `.../site/3.0`, `.../site`, `$PREFIX/share/guile`            | `GUILE_LOAD_PATH`           | `%load-path`, `(%site-dir)`                            |
| Sagittarius    | `$PREFIX/share/sagittarius/sitelib`, then versioned directories                         | `SAGITTARIUS_LOADPATH`      | `(load-path)`                                          |
| Chicken 6.0.0  | `$PREFIX/lib/chicken/12`                                                                | `CHICKEN_REPOSITORY_PATH`   | `(repository-path)`, `chicken-install -repository`     |
| Chez           | `.` only                                                                                | `CHEZSCHEMELIBDIRS`         | `(library-directories)`                                |
| Racket 9.3     | Per-user collects, then the installation's collects                                     | `PLTCOLLECTS`               | `(current-library-collection-paths)`                   |
| Gambit 4.9.8   | `~~userlib` (`~/.gambit_userlib`), then `~~lib` (`$PREFIX/lib`)                         | `GAMBOPT` with `search=DIR` | `(path-expand "~~userlib")`                            |
| Meevax         | `$XDG_DATA_HOME/meevax` (else `~/.local/share/meevax`), `$PREFIX/share/meevax`          | None                        | `meevax --library-directories`                         |

The last column lists ways to query each implementation, which are not always what snow-chibi uses. For example, snow-chibi installs Guile packages into `(%site-ccache-dir)`. For other implementations snow-chibi supports, it queries `(install-path #:libdir)` on STklos, `(scheme-library-paths)` on Ypsilon, `(scheme-paths)` on TR7, `(Cyc-installation-dir 'sld)` on Cyclone, and `(system-library-directory-pathname)` on MIT Scheme.

### Search order

Measured with an `-I` directory, an environment variable, and an `-A` directory all set:

- Gauche: `-I`, `GAUCHE_LOAD_PATH`, system directories, `-A`.
- Guile: `-L`, `GUILE_LOAD_PATH`, system directories.
- Chibi 0.12.0: `-I`, system directories, `CHIBI_MODULE_PATH`, `-A`. Its man page documents the environment variable before the system directories, so the documentation and the behavior disagree.
- Sagittarius: `SAGITTARIUS_LOADPATH` comes before the system directories.

### Standards

- [SRFI 138](https://srfi.schemers.org/srfi-138/srfi-138.html) defines `-I` (prepend) and `-A` (append) and leaves the default directory list to each implementation.
- [SRFI 176](https://srfi.schemers.org/srfi-176/srfi-176.html) defines a `-V` output whose `scheme.path` property lists the directories searched for libraries, highest priority first. Gauche implements it. snow-chibi does not read it for any implementation.
- R7RS says nothing about where library files live.

### Akku

[Akku](https://gitlab.com/akkuscm/akku), the other Scheme package manager, installs into a project-local `.akku/lib` and generates an `.akku/env` script. That script exports each implementation's own variable (`CHIBI_MODULE_PATH`, `GAUCHE_LOAD_PATH`, `CHEZSCHEMELIBDIRS`, `GUILE_LOAD_PATH`, `SAGITTARIUS_LOADPATH`, and others) and appends the shared `R6RS_PATH` and `R7RS_PATH` variables so that globally installed libraries remain reachable.

## Conventions

- System directories are derived from the installation prefix at build time, usually `$PREFIX/share/<name>` with a separate site directory for third-party libraries.
- Implementations installed per user, or those that want installs without root, add a per-user directory: Meevax uses the XDG data directory, Gambit uses `~/.gambit_userlib`, and Racket uses a per-user collects directory. Gambit and Racket search it before the system directory, and Meevax lists it first.
- Most implementations have an environment variable with colon-separated directories, usually named like `<NAME>_LOAD_PATH`.
- Directories that do not exist are skipped. Meevax raised an error at first and changed it after the snow-chibi report.
- The current directory is rarely a default. Chibi includes `./lib` and `.`, and Chez includes only `.`.

## Recommendation

1. Search `-I` directories, then `STAK_LIBRARY_PATH` (colon-separated), then `$XDG_DATA_HOME/stak` (or `~/.local/share/stak`), then a system directory such as `/usr/local/share/stak`, then `-A` directories. This matches Gauche and Guile.
2. List the user directory before the system directory. Stak is installed with `cargo install`, so there is no installation prefix, and snow-chibi installs into the first directory it is given. A user directory first means installs without sudo, the same choice Meevax made.
3. Add a command-line flag, handled in `run.scm`, that prints the directories one per line, in search order. The existing `(library-paths)` procedure in `(stak compile)` would return the same list. snow-chibi can reuse its Meevax code to find the install directory and to run tests, but its feature probe needs a temporary file, as Loko's does, because Stak has no `-e` flag.
4. Put the defaults in the interpreter and REPL drivers (`run.scm` and `repl.scm`), not in the compiler frontend. `stak-compile` and `stak-build` then keep producing the same bytecode regardless of what is installed on the machine, and embedded builds are unaffected. `mstak` shares `run.scm` but cannot read environment variables, so it can search only the system directory.
5. Keep skipping directories that do not exist.

SRFI 176 is the standardized alternative to a custom flag, but it would replace clap's `-V` handling in `stak_sac::main!`, which every binary built with that macro shares.

## Decisions only the maintainer can make

- The names of the environment variable and the flag. The code already says "library paths", so `STAK_LIBRARY_PATH` and `--library-paths` would be consistent.
- The system directory: a fixed `/usr/local/share/stak`, one that packagers set at build time, or snow-chibi's fallback `/usr/local/share/snow/stak`. The fallback lets `snow-chibi install --impls=stak` work before snow-chibi supports Stak, after a confirmation prompt and without package tests, but installs there usually need root.
- Whether `STAK_LIBRARY_PATH` should redirect snow-chibi installs. If the flag prints the search order, the variable's first directory becomes the install directory, as `CYCLONE_LIBRARY_PATH` does for Cyclone. Gauche instead reports its install directory separately with `gauche-config --sitelibdir`.
- Whether `stak-compile` should also search the default directories.
- Whether to append the shared `R7RS_PATH` variable that Akku uses.

## Side findings

These are separate from the issue but affected snow-chibi integration at `32410b5a4`. All three have been fixed on `main` since.

1. `exit` treats only `#t` or no argument as success. `(exit 0)` and `(exit 3)` both print `Error: halt` and exit with status 1. snow-chibi's test runner fails any package test that exits with an integer, though tests ending with `(test-exit)` from `(chibi test)` pass because it exits with a boolean. The SRFI probe also ends with `(exit 0)`, but snow-chibi ignores the probe's exit status. Chibi and Gauche pass integer arguments through as exit codes, and R7RS asks implementations to translate the argument into an appropriate exit value. [#4143](https://github.com/raviqqe/stak/pull/4143) fixed it.
2. `cond-expand` checks `(library ...)` requirements against a list fixed by `define-features` around line 334 of `prelude.scm`: `(scheme base)`, `(scheme read)`, `(scheme write)`, `(stak base)`, `(stak continue)`, and `(stak exception)`. `(srfi 1)`, `(scheme char)`, and every library on the load path test false, and `(features)` has no `srfi-N` entries. snow-chibi would conclude that Stak has no built-in SRFIs and install snow-fort's `(srfi 1)`, which Stak's built-in library would then shadow because built-in libraries resolve before the load path. [#4141](https://github.com/raviqqe/stak/pull/4141) fixed it by checking built-in libraries and `.sld` files on the load path instead. snow-chibi's probe now finds SRFI 1 and skips installing it as a natively implemented library, but the built-in `(srfi 1)` is partial and lacks procedures such as `filter-map`, `take`, and `partition`.
3. `include-library-declarations` inside `define-library` fails with `procedure expected`. [#4144](https://github.com/raviqqe/stak/pull/4144) added it.

## Sources

- [stak#4100](https://github.com/raviqqe/stak/issues/4100)
- [stak#4141](https://github.com/raviqqe/stak/pull/4141), [stak#4143](https://github.com/raviqqe/stak/pull/4143), and [stak#4144](https://github.com/raviqqe/stak/pull/4144)
- [snow-chibi source](https://github.com/ashinn/chibi-scheme/tree/master/lib/chibi/snow), including the commit adding Meevax support ([`d44eb977a`](https://github.com/ashinn/chibi-scheme/commit/d44eb977a))
- [meevax#494](https://github.com/yamacir-kit/meevax/issues/494), [meevax#501](https://github.com/yamacir-kit/meevax/issues/501), and [meevax#505](https://github.com/yamacir-kit/meevax/issues/505)
- [SRFI 138](https://srfi.schemers.org/srfi-138/srfi-138.html) and [SRFI 176](https://srfi.schemers.org/srfi-176/srfi-176.html)
- [Akku's `install.scm`](https://gitlab.com/akkuscm/akku/-/blob/master/lib/install.scm)
