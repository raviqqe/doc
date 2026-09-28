# Big integers in Stak Scheme without Rust arithmetic

<!-- cspell: ignore adigit adigits bigit bigits bignum bignums bnsimplify burnikel expt fdigit fixnum fixnums ikarus immediates karatsuba loko mdigit mdigits nmath nonbox oaklisp octocov picobit precheck sbcl taocp univlib varints vicare zenlisp zimmermann -->

- Date: 2026-09-22
- Repository: [`raviqqe/stak`](https://github.com/raviqqe/stak)
- Version: `main` at `f4e83a5c2`

## Summary

- Arbitrary-precision integers can be represented as ordinary data ribs with an unused type tag and implemented entirely in the prelude. The VM already treats such ribs as inert: the garbage collector copies them, `equal?` compares them structurally, and nothing in Rust inspects the tag.
- The one prelude and one bytecode format serve three number representations (63-bit integer, `f64`, and the `float62` NaN-box), so the fixnum bound must be the weakest build's exact range, plus or minus 2^53, and digits must be at most 26 bits wide (or a decimal base of 10^7) so that a digit product plus two carries stays exact everywhere.
- Four designs are viable. An opt-in `(stak bignum)` library costs nothing to programs that do not import it. Transparent promotion (fixnums overflow into bignums automatically) costs 25-55 % on arithmetic-bound benchmarks if the overflow test is expressed as `(or ($+ x y) (slow+ x y))` in Scheme, and 1.6-2.5x if expressed as explicit range checks, because the VM allocates a heap cell per stack push and closes `let` bindings with a primitive call. A ~60-line VM change in which the primitive itself transfers control to a Scheme procedure on overflow makes transparent promotion free on the fast path while keeping all bignum arithmetic in Scheme.
- Two side findings: the batch compiler mis-encodes negative integer literals beyond 2^51, and `let` bindings are closed with a `$$unbind` primitive call where a zero-allocation `set 1` instruction already exists in the compiler.

## The numeric core today (verified)

### Representations

| build                                   | crate feature | number payload                               | exact integer range                                                                   | source                            |
| --------------------------------------- | ------------- | -------------------------------------------- | ------------------------------------------------------------------------------------- | --------------------------------- |
| integer (default of `stak-vm`, `mstak`) | none          | `i64` boxed as `(n << 1) \| 1`               | 63 bits, wraps silently                                                               | `vm/src/value_inner/integer63.rs` |
| float (the `stak` binary)               | `float`       | `f64`                                        | integers exact to 2^53, rounds beyond                                                 | `vm/src/value_inner/float64.rs`   |
| float62                                 | `float62`     | `nonbox::Float62`, 63-bit integers or floats | 63 bits; `+ - *` wrap (nonbox `wrapping_*`), `expt` falls back to a float on overflow | `vm/src/value_inner/float62.rs`   |

Numbers are unboxed immediates and every other value is a rib (cons) whose cdr carries a 16-bit tag (`vm/src/cons.rs`, `Tag = u16`). `number?` in `(stak base)` is `(not (rib? x))`. Tags in use: 0 pair, 1 null, 2 boolean, 3 procedure, 4 symbol, 5 string, 6 character, 7 vector, 8 bytevector, 9 record, `u16::MAX` foreign (`vm/src/type.rs`). Rust code only inspects pair, null, boolean, symbol, string, character, procedure and foreign.

### Arithmetic

`(stak base)` binds the primitives `$<` 10, `$+` 11, `$-` 12, `$*` 13, `$/` 14, `remainder` 15, `expt` 16, `quotient` 80 and `sqrt` 504; `(scheme inexact)` binds 500-510. The ids are assigned in `r7rs/src/small/primitive.rs` and dispatched by offset from `r7rs/src/small.rs` to `native/src/arithmetic.rs` (`quotient`, defined as `(x - x % y) / y`) and `inexact/src/primitive_set.rs`. `Primitive::ADD` and friends live in `r7rs/src/small.rs` and go through `Memory::operate_binary` -> `pop_numbers` -> `assume_number`: a rib reaching them is only caught by a debug assertion, so release builds reinterpret its bits. Any bignum design must type check before calling a primitive, never after.

Generic `+` and `*` are `fold`s over the primitives (`arithmetic-operator`, prelude.scm ~766), and `-` and `/` use `inverse-arithmetic-operator`, which special-cases the one-argument form; `=` is `(comparison-operator eq?)`; `<` and friends wrap `$<`. `define-optimizer` rewrites binary calls to the primitives at compile time (prelude.scm ~780). A user library that exports its own `+`, `*`, `=` and is imported with `(except (scheme base) + * =)` is called as written on both the eval path and the batch path (verified), so the optimizer does not capture shadowing exports.

`exact` is `round`, `inexact` is the identity, `exact?` is `integer?` (`(and (number? x) (zero? (remainder x 1)))`, whose guard is what keeps a rib away from the `remainder` primitive), and `exact-integer?` is `(and (exact? x) (integer? x))` (prelude.scm ~853-903). No build has an exactness bit visible to Scheme: `Value::eq` compares numbers numerically, so even float62's internal int/float distinction is invisible (`(eq? 1.0 1)` is true there).

### Observed overflow behavior

Float build (`target/release/stak`, eval path):

| expression                                 | result                           | comment                                  |
| ------------------------------------------ | -------------------------------- | ---------------------------------------- |
| `(* 4611686018427387903 2)`                | `9223372036854776028`            | rounded; the true value is ...5806       |
| `(expt 2 64)`                              | `18446744073709552046`           | rounded                                  |
| `(+ 9007199254740992 1)`                   | `9007199254740992`               | 2^53 + 1 is not representable            |
| `(exact-integer? 9007199254740993)`        | `#t`                             | any integral float passes                |
| `123456789012345678901234567890`           | `123456789012345648200086220240` | literal parsed by f64 accumulation       |
| `(string->number "123456789012345678901")` | `123456789012345706468`          |                                          |
| `(number->string 9223372036854775808)`     | `"9223372036854778606"`          | the integer-digit loop drifts above 2^54 |
| `(exact (expt 2 70))`                      | `1180591620717411468064`         | `exact` is `round`                       |

Integer build (bytecode from `stak-compile`, run with `mstak-interpret`; the compiler is a float build, so large literals are already inexact and big values must be built arithmetically):

| expression                                         | result                                                                               |
| -------------------------------------------------- | ------------------------------------------------------------------------------------ |
| `(+ m 1)` with `m = 2^62 - 1` built arithmetically | `-4611686018427387904` (wraps to the minimum, which `number->string` prints as `-,`) |
| `(+ m 2)`                                          | `-4611686018427387903`                                                               |
| `(* m 2)`                                          | `-2`                                                                                 |
| `(* 3037000500 3037000500)`                        | `145474192`                                                                          |
| `(expt 2 63)`                                      | `0`                                                                                  |
| `(string->number "12345678901234567890")`          | `3122306864379792082`                                                                |

The `release_test` profile enables `overflow-checks`, so `*` and `expt` on the integer build presumably panic there instead of wrapping (not verified).

### How an unknown tag behaves (verified with `(rib 1 '(2 3) 10)`)

| operation                                    | result                                             |
| -------------------------------------------- | -------------------------------------------------- |
| `(rib-tag x)`                                | `10`                                               |
| `(car x)`, `(cdr x)`                         | `1`, `(2 3)`                                       |
| `(number? x)`, `(record? x)`, `(pair? x)`    | `#f`                                               |
| `(eqv? x y)` for a structurally equal `y`    | `#f` (the primitive only special-cases characters) |
| `(equal? x y)`                               | `#t`; `#f` when the digits differ                  |
| survives a 20,000-cell allocation burst (GC) | yes                                                |
| `(write x)`                                  | error "unknown type to write"                      |

`(stak base)` exports `rib`, `rib?`, `rib-tag`, `data-rib`, `instance?` and `primitive`, so a new type is `(define bignum-type 10)` next to `(define record-type 9)`.

### Literal pipeline

The reader (`(scheme read)`, prelude.scm ~6205) does `(or (string->number x) (string->symbol x))`; `string->number` (`(stak string)` ~1896) accumulates digits with `+` and `*` in the host representation; the compiler's `encode-number` (compile.scm ~1965) writes integers as little-endian varints of arbitrary length, and `Vm::decode_number` (vm/src/vm.rs ~523) folds the `u128` into `Number::from_i64` or `from_f64`. `marshal-rib` (compile.scm ~1683) accepts only the known constant types and raises "invalid type" for other ribs, while `encode-rib` serializes any rib generically with its car, cdr and tag. A bignum constant therefore needs one new case in `marshal-rib` and nothing in the VM: it decodes as a data rib whose digits are small fixnums.

### Cost model

Tree shaking (`shake-tree`, compile.scm ~1430) is a static closure over top-level definition dependencies: an unimported library costs nothing, and anything reachable from `+` is paid by every program. CI records bytecode size as an octocov metric (`tools/bytecode_size_bench.sh` compiles every tracked `.scm` outside `prelude`/`tools`) and CodSpeed reports timing on every pull request; `bench/src/{fibonacci,tak,sum,add}` are pure fixnum arithmetic. Prelude bytecode density is roughly 10-13 bytes per source line: referencing `number->string` (50 lines plus transitively reachable helpers) adds 637 bytes to the 1,632-byte bytecode of `(import (scheme base)) (write-u8 65)`.

`doc/src/content/docs/limitations.md` documents the 63-bit/f64 limitation. No issue or pull request mentions bignums or arbitrary precision (the only hits for `bigint` are dependency changelogs quoted in dependabot pull requests).

## Prior art: bignums implemented in Scheme

Sources were fetched and read by a research agent; Gambit, Owl, Larceny and Loko were read from the raw files, the rest through a summarizing fetcher. Two common beliefs turned out to be wrong: Gambit does not use Burnikel-Ziegler division, and Scheme 48's bignums are C, not PreScheme.

### Comparison

| system (license)                  | file                                                             | digit                                                                                                                                             | storage, order, sign                                                                            | multiplication                                                                                           | division                                                                                                                            | radix conversion                                                      | size                                |
| --------------------------------- | ---------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------- | ----------------------------------- |
| Gambit (LGPL 2.1 / Apache 2.0)    | `lib/_num.scm`, `_num#.scm`, `_univlib.scm`                      | 64/32-bit "adigits" and 32/16-bit "mdigits" on C; 14-bit adigit/mdigit and 7-bit fdigit under 30-bit fixnums on the universal (JS/Python) backend | vector, little-endian, two's complement with the top bit of the last adigit as sign             | schoolbook below 1400 bits, Karatsuba, complex-double FFT from 20000 bits (disabled on JS for code size) | Knuth D (`naive-div`) plus a Newton reciprocal for very large divisors; single-mdigit fast path                                     | divide and conquer over squared powers of the radix                   | ~4,400 of 13,567 lines (FFT ~2,900) |
| Owl Lisp (MIT)                    | `owl/math.scm`, VM `c/ovm.c`                                     | full 24-bit fixnum; VM opcodes give the 48-bit product halves and a two-digit-by-one-digit divide                                                 | chain of typed pairs (`ncons`), least significant first, sign in the head's type tag            | schoolbook + Karatsuba (split >= 30 digits)                                                              | big / digit via `fxqr`; big / big by shift-and-subtract (author: "ugly and slow")                                                   | repeated `truncate/`                                                  | ~1,600 lines                        |
| Larceny (permissive, attribution) | `src/Lib/Common/bignums.sch`, `bignums-el.sch`, `bignums-be.sch` | 16-bit half-bigits under 30-bit fixnums (storage is 32-bit)                                                                                       | bytevector-like, sign byte + 24-bit length, little-endian; zero has length 0                    | schoolbook + Karatsuba (> 10 bigits)                                                                     | Knuth D (`slow-divide`, `d = floor(b / (v1 + 1))`, add-back "called only with very low probability") and single-bigit `fast-divide` | one division per output digit                                         | 1,431 + 921 + 881 lines             |
| Loko (EUPL 1.2)                   | `runtime/arithmetic.sls`                                         | 30 bits under 61-bit fixnums (`2w + 1` must fit a fixnum)                                                                                         | boxed {used, sign, vector}, little-endian, sign-magnitude, `clamp!` and `bnsimplify!` normalize | O(n^2) only                                                                                              | schoolbook (HAC 14.20 / LibTomMath style) with power-of-two normalization                                                           | divide and conquer with squared powers; bit fields for bases 2, 8, 16 | ~1,240 of 4,017 lines               |
| Oaklisp (GPL 2)                   | `src/world/bignum.oak`                                           | base 10^4 (32-bit) or 10^9 (64-bit): the largest power of ten such that `(B-1)^2 + 2(B-1)` is a fixnum                                            | list, little-endian, sign-magnitude                                                             | schoolbook below 16 digits, divide and conquer above                                                     | long division with a leading-digit estimate                                                                                         | trivial chunking                                                      | ~650 lines                          |
| zenlisp (Holm; do what you want)  | `nmath.l`, `imath.l`                                             | base 10, one symbol per digit                                                                                                                     | list of digit symbols                                                                           | repeated addition                                                                                        | repeated subtraction                                                                                                                | trivial                                                               | ~550 lines                          |

Not Scheme-level: Ribbit (no bignums at all), Ikarus/Vicare (C, GMP `mpn`), Scheme 48 (`c/bignum.c`), PICOBIT (C), Guile Hoot (host `BigInt` / mini-gmp). SBCL's `src/code/bignum.lisp` (public domain, word-size two's-complement digits, Knuth division, binary gcd) is the best-documented Lisp-level reference.

### Details worth copying

- Gambit's fixnum dispatch is `(or (##fx+? x y) (##bignum.+ ...))` where the backend primitive returns `#f` on overflow; on JS it re-narrows with shifts and compares. `##bignum->fixnum?` folds digits from the top with overflow-checking `##fx*?`/`##fx+?` to decide whether a result fits. `##exact-int.sqrt` follows Zimmermann's "Karatsuba square root"; `expt` is square-and-multiply; gcd switches to a recursive half-gcd above 1400 bits.
- Owl's chain of typed pairs with the sign in the head's tag is the closest existing analog to a rib-only heap. Its weak spot is big-by-big division.
- Larceny's `big2*+` (`t = a[i] * b[j] + c[i+j] + carry` split into halves) is the schoolbook inner step for half-width digits, and its `slow-divide` is the most compact Knuth D in the survey.
- Loko's `display-int` (divide and conquer with a table of squared powers) and `string->number` (Horner) are clean references for conversion.
- Every implementation except Gambit uses sign-magnitude. R7RS-small has no bitwise operations, so two's complement buys nothing in Stak.

### Algorithmic references

- Digit width (Knuth TAOCP vol. 2 §4.3.1; Brent and Zimmermann, Modern Computer Arithmetic, ch. 1): the schoolbook step is bounded by `(β-1)^2 + 2(β-1) = β^2 - 1`, and Knuth D needs `(u_j β + u_{j+1}) < β^2`. With 53 exact bits, `w = 26` (products below 2^52 leave room for carries); with 62 bits, `w = 31` (Loko uses 30 under 61-bit fixnums). Decimal alternatives: 10^7 on a double host, 10^9 needs 60 bits.
- Knuth D normalization: scale so that the divisor's top digit is at least `β/2` (`d = floor(β / (v_{n-1} + 1))` or a left shift by the leading zero count); the estimate `q̂ = min(β-1, floor((u_{j+n} β + u_{j+n-1}) / v_{n-1}))` is never too small and at most two too large; the extra test against `v_{n-2}` makes it at most one too large; add back once if the multiply-subtract goes negative.
- Fixnum overflow without hardware flags (CERT INT32-C): addition overflows iff `(b > 0 and a > MAX - b) or (b < 0 and a < MIN - b)`; multiplication by sign cases with truncating division, e.g. `a > 0, b > 0: a > MAX / b`. On a double host, `+`/`-` results of operands within 2^53 are ordered correctly after rounding, so a range check on the result is sound; products are not, so use the division precheck or the 26-bit magnitude bound.

## Design space for Stak

- Fixnum bound: `limit = 2^53`, the same constant on every build, because one bytecode runs on all VMs and the float build cannot represent integers above 2^53 exactly. Sums of two fixnums stay below 2^54, which every build holds.
- Digits: 26-bit binary (`β = 2^26`), least significant first, or decimal 10^7 if trivial radix-10 conversion matters more than binary shifts. `quotient` and `remainder` are exact on all builds for integers below 2^53, so digit splitting needs no bit operations.
- Storage: `(rib sign digits bignum-type)` with `bignum-type = 10`; sign as a boolean or +-1 in the car, digits as a list in the cdr. Lists suit add/sub/mul directly; Knuth D can work on list windows or on the `(stak vector)` radix tree if indexed access proves necessary.
- Canonical form: no leading zero digits and magnitude above the fixnum bound, so that `equal?` is structural equality and every result normalizes back to a fixnum when it fits.
- Algorithms for a first version: schoolbook add/sub/mul, Knuth D division, Euclid gcd via `quotient`, Newton `exact-integer-sqrt`, square-and-multiply `expt`, chunked radix conversion (divide by 10^7 or multiply-add by 10^7). Karatsuba is optional later (thresholds in the survey: 10-30 digits).
- Mixed arithmetic: bignum with float converts the bignum by Horner over its digits; `sqrt` of a bignum goes through the float conversion, and `exact-integer-sqrt` stays exact.
- Where the code lives: transparent variants must sit inside `(stak base)` because `+` is defined there and libraries must precede their importers; conversions belong in `(stak string)`; an opt-in library can live anywhere after `(stak base)`.

## Candidates

### Candidate 1: opt-in `(stak bignum)` library, no promotion

A library defining bignums as ribs with a fresh tag (or a record type) and exporting its own `+ - * quotient remainder = < > <= >= gcd lcm expt exact-integer-sqrt number->string string->number` that accept fixnums and bignums and return a fixnum whenever the result fits. Users import it with `(except (scheme base) ...)`. Big values come from `string->number` or arithmetic; there are no bignum literals.

- Cost to everyone else: zero. Nothing changes in `(stak base)`, the reader, the writer or compile.scm. `(scheme write)` would need a `bignum?` branch if `write` is to print them (a few lines).
- Drawback: not R7RS-transparent.
- Size: ~600-900 lines of Scheme (the Oaklisp/Owl code shape).

### Candidate 2: transparent promotion, overflow detected in Scheme

Redefine `+ - * quotient remainder expt` and their optimizer templates as a fixnum fast path with inlined range checks that fall back to the library from candidate 1; results normalize back to fixnums. `number?` becomes `(or (not (rib? x)) (bignum? x))`; `=` and `<` dispatch; `write`, the reader, `string->number` and `marshal-rib` learn the new type.

- Cost: 1.6-2.5x on arithmetic micro-benchmarks ([Fast-path overhead measurements](#fast-path-overhead-measurements)), ~10 KB of bytecode in every program (estimate from [Cost model](#cost-model)), and the whole bignum core joins `(stak base)`.

### Candidate 3: transparent promotion, overflow detected by the VM

`$+ $- $* expt` return `#f` on overflow or non-fixnum arguments (a `checked_add`-sized change per representation in `vm/src/number.rs` and `vm/src/value_inner/*`), and the optimizer emits `(or ($+ x y) (slow+ x y))`.

- Cost: the `or` shape adds 3-4 instructions and 2-3 heap cells per operation on the fast path ([Fast-path overhead measurements](#fast-path-overhead-measurements)): 1.25-1.55x on arithmetic micro-benchmarks. Same bytecode-size cost as candidate 2.
- The VM-side variants X and Y in [VM-side alternatives that keep the fast path bare](#vm-side-alternatives-that-keep-the-fast-path-bare) remove the Scheme-side cost.

### Candidate 4: overflow error by default, promotion when imported

The fast path of candidate 2 or 3, but the slow path is a mutable hook that `(stak base)` initializes to `(error "integer overflow")` and `(stak bignum)` replaces on import. Programs that do not import it pay only the check, not the bignum code, and overflow becomes a loud error instead of a silent wrong answer. Combined with alternative X this is the cheapest fully transparent design.

## Fast-path overhead measurements

Question: does `(or ($+ x y) (slow+ x y))` cost anything on the Scheme side when the VM never returns `#f`? Method: four shapes of each benchmark, compiled with `stak-compile` and run under `hyperfine` on both release interpreters. The VM primitive cannot return `#f` today, so shape C uses `(if z z (error ...))` with the branch always taken the same way. `stak-decode` confirms that `(or ($+ x y) (slow+ x y))` and `(let ((z ($+ x y))) (if z z (slow+ x y)))` compile byte-identically, and that shape C has the same fast-path instructions as the literal `or` version (only the else branch differs); the two also time identically.

- A bare: `(+ x y)`
- B let-only: `(let ((z (+ x y))) z)`
- C candidate 3: `(let ((z (+ x y))) (if z z (error "overflow")))`
- D candidate 2: `(let ((z (+ x y))) (if (< z limit) (if (< negative-limit z) z (error ...)) (error ...)))`

### Instruction accounting (from `stak-decode`, vm.rs and memory.rs)

The VM has five instructions: constant, get, set, if, call (`vm/src/instruction.rs`). `get` and `constant` each allocate one stack cell (`push` is a cons); `if` allocates nothing (it reuses the popped test cell as the jump cell, vm.rs ~218); a primitive call allocates one cell for its result; `$$unbind` is an ordinary primitive call (primitive 2, r7rs/src/small.rs ~128) dispatched through a global symbol and elided only in tail position (`compile-unbind`, compile.scm ~1091).

| shape, non-tail position                                | instructions                                   | primitive dispatches | cells allocated | delta vs A                      |
| ------------------------------------------------------- | ---------------------------------------------- | -------------------- | --------------- | ------------------------------- |
| A `(+ x y)`                                             | get, get, call $+                              | 1                    | 3               | -                               |
| B let-only                                              | get, get, call $+, get, call $$unbind          | 2                    | 5               | +2 instr, +1 dispatch, +2 cells |
| C candidate 3                                           | get, get, call $+, get, if, get, call $$unbind | 2                    | 6               | +4 instr, +1 dispatch, +3 cells |
| reference `(if (+ x y) 1 2)` (branch without rebinding) | get, get, call $+, if, constant                | 1                    | 4               | +2 instr, +1 cell               |

In tail position `$$unbind` disappears: C is +3 instructions and +2 cells. Candidate 2's shape D adds two global reads, two `$<` dispatches and a nested `if` on top of C.

### Timings (`hyperfine -N`, min ms; ratios are min/min within the same run)

| benchmark     | build   | A     | B let-only    | C candidate 3 | D candidate 2 |
| ------------- | ------- | ----- | ------------- | ------------- | ------------- |
| fib 27        | float   | 38.2  | 47.5 (1.24x)  | 55.2 (1.44x)  | 78.6 (2.06x)  |
| fib 27        | integer | 33.8  | 39.9 (1.18x)  | 46.4 (1.37x)  | 74.1 (2.19x)  |
| tak 16 8 0    | float   | 151.1 | 178.7 (1.18x) | 191.6 (1.27x) | 254.9 (1.68x) |
| tak 16 8 0    | integer | 148.7 | 171.6 (1.15x) | 185.1 (1.24x) | 239.2 (1.61x) |
| sum 3,000,000 | float   | 167.4 | 221.2 (1.32x) | 255.4 (1.53x) | 429.5 (2.49x) |
| sum 3,000,000 | integer | 157.2 | 210.2 (1.34x) | 238.3 (1.52x) | 386.7 (2.46x) |

Provenance: the float rows' A-C cells, the float fib D cell and the integer fib A-C cells are the adversarial reproduction (3 warm-ups, 20-40 runs on a quiet machine); the integer tak and sum rows and the remaining D cells are the original measurement passes (30 runs on the float build, 15 + 40 runs on the integer build), with their ratios computed against the A of their own pass (float tak 152.0 ms, float sum 172.6 ms). The original passes and the reproduction agree within 9 %, most cells within 2 %; the largest deviation, float sum B at 241 versus 221 ms, coincided with machine load. Every shape prints the same result. Bytecode sizes for fib: A 3,990, B 4,002, C 4,047, D 4,084 bytes (tak and sum grow by similar amounts); the growth in C and D is the "overflow" string literal (`error` is already in A's bytecode).

Decomposition of C over A: binding plus `$$unbind` ~9 ns per operation, `if` plus re-read ~6 ns, on top of ~15 ns for the bare addition. In fib one of the three wrapped sites is in tail position, so the branch's share is 45-52 % there and 32-39 % in tak and sum.

Confounders checked and rejected: heap size (C - A on fib is 17.0 ms at the default 4M-word heap, 17.5 ms at 64M and 17.2 ms at 512K), the reachability of `error` (a shape with `error` reachable but no `if` times exactly like B), and the spelling (the literal `or` macro and a `0` else-branch time exactly like C).

Earlier hand measurements in this study were more pessimistic (a checked `+` implemented as closures from a user library was ~10x; an inline check that recomputed the negative bound and used the three-argument `<` was 4x); the table above supersedes them.

### An incidental compiler finding

`compile-let` closes every binding with `call $$unbind`, although the compiler already has a zero-allocation unbind, `compile-unsafe-unbind` (`set 1`, compile.scm ~1113), used only for the `$procedure` temporary. A scratch copy of compile.scm bootstrapped through `stak-interpret` and switched to `set 1` for all lets cut B from 1.24x to 1.12x and C from 1.43x to 1.31x on fib, and from 1.32x to 1.20x and 1.53x to 1.43x on sum, with unchanged outputs. Whether `set 1` is safe for every `let` shape (it mutates the stack cell in place) was not examined; it is independent of bignums.

## VM-side alternatives that keep the fast path bare

Both keep every digit of bignum arithmetic in Scheme and leave the compiled fast path as `get, get, call $+`.

### X: the primitive invokes the slow path (feasible today)

`PrimitiveSet::operate` only receives `&mut Memory`, but it can make the next interpreter iteration perform a call by splicing code ribs in front of the next instruction, which is exactly how `run_async` invokes the runtime error handler (vm.rs ~123-153). On overflow the `ADD` arm re-pushes the operands, builds `(0 . call-rib)` whose call rib is `(hook-cell . next)` with the call tag for two arguments and `next = cdr(code)`, and sets `code` to it. The hook's return value lands where the primitive's result would have; when `next` is null the hook is tail-called with the current frame's continuation. Every `allocate` may collect, so pointers must be re-read from the heap or parked in `register` between allocations, and the profiler expects the callee cell to look like a symbol.

How the primitive finds the hook:

| option                                 | mechanism                                                                             | Rust touched    | fast-path cost             |
| -------------------------------------- | ------------------------------------------------------------------------------------- | --------------- | -------------------------- |
| a. extra argument `($+ x y $slow+)`    | optimizer emits a 3-argument call                                                     | `small.rs` only | +1 `get`, +1 cell per site |
| b. singleton slot `(set-car! #t hook)` | `(car #t)` and `(car '())` are unused; precedent: the error handler in `(cdr '())`    | `small.rs` only | zero                       |
| c. registration primitive + GC root    | new `hook` field in `Memory`, copied in `collect_garbages`, a `Primitive::SetSlowAdd` | ~18 lines       | zero                       |
| d. symbol table at decode time         | symbols' names are stripped after compilation                                         | -               | infeasible                 |

Estimate for `+` alone: 20-30 lines of checked arithmetic across `number.rs` and the three `value_inner` files, ~20 lines for the splice helper (or one `pub use` of `Instruction` plus ~15 lines in `small.rs`), 10-15 lines in the `ADD` arm: about 50-70 lines of Rust, roughly doubled to cover `-` and `*` as well (`expt` was not estimated). Hazards: an uninstalled hook halts with "procedure expected"; a nested overflow inside the hook recurses, so the bignum code must be fixnum-safe; `quotient` and `$<` on bignum ribs remain Scheme-side dispatch.

### Y: continuable overflow exception (not viable as-is)

Primitive errors are non-continuable by prelude convention (prelude.scm ~2571: the installed handler marks them so, and `convert-exception-handler` raises if a handler returns). At the VM level a returning handler re-executes the failing instruction rather than resuming after it, because the synthetic call rib's cdr is `code` (vm.rs ~129-141); this was confirmed empirically on both builds with a handler installed through `(set-cdr! '() ...)`. Making Y correct needs a non-critical `Error::Overflow`, a non-popping checked operation, and a resume point of `cdr(code)` in `run_async`: 60-75 lines, the same order as X, and it ends up being X routed through the error path. Routed through `with-exception-handler` it would cost roughly ten times X's slow path and any user `guard` between the handler and the `+` would silently swallow overflows.

## Decisions only the maintainer can make

- Exactness on the float builds. Transparent promotion has to define "exact integer" as "integral immediate within plus or minus 2^53, or a bignum", so `(* 1e10 1e10)` would yield an exact bignum. This is no worse than today's `exact? = integer?` fiction, but it would be codified. The integer build has no such ambiguity. Any immediate outside the fixnum range is then, by definition, inexact.
- `eqv?`. R7RS wants `(eqv? big1 big2)` to be numeric equality; the primitive only special-cases characters. Either wrap `eqv?` in Scheme (taxing `memv`, `assv` and `case` everywhere) or document identity semantics for bignums.
- Digit base: binary 2^26 versus decimal 10^7.
- compile.scm's float encoder evaluates `(expt 2 y)` for `y` up to 1023; under transparent promotion those become bignums and `(/ x (expt 2 y))` becomes float divided by bignum. It works given bignum-to-float conversion, but it is a deliberate test case.
- Whether the ~60 lines of Rust in alternative X are acceptable under the "not in Rust" goal, given that they contain no bignum arithmetic.

## Recommendation

1. Build candidate 1 as `(stak bignum)`: 26-bit sign-magnitude digits in a least-significant-first list under tag 10, schoolbook add/sub/mul, Knuth D, Euclid gcd, Newton isqrt, chunked radix conversion. It ships without touching the size or timing metrics, it is what every other candidate needs anyway, and it can be validated with Gherkin scenarios run differentially against Gauche, Chibi and Guile through `tools/integration_test.sh`.
2. Fix the negative-literal encoding bug ([Side findings](#side-findings)) first; it sits on the same path bignum literals would use.
3. If transparent promotion is wanted, prefer alternative X with hook option b or c, combined with candidate 4's "error unless imported" default. Avoid candidate 2 unless the Rust line is absolute, and avoid Y.
4. Consider the `set 1` unbind change separately; it is a free 10-12 % on let-heavy code.

## Side findings

- Negative literal encoding bug (verified through `stak-compile` + `stak-interpret`): `encode-number` computes `4|x| + 1` for negative integers, which is not representable in f64 once |x| > 2^51. `-2251799813685249` prints as `4503599627370498` and `-9007199254740992` as `18014398509481984`; `-2251799813685247` (the largest magnitude used by `number.feature`) and positive literals (encoded as `2x`) are fine up to 2^53. The eval path keeps literals as live values and is unaffected.
- `(stak base)` does not export `$+`, `$-` and the other primitive names; user code naming them compiles to an unbound global and fails with "procedure expected". Benchmarks and prototypes must go through `+` and the optimizer.
- `stak-profile run` hung in the sandbox used for this study (killed after 300 s); instruction counts were taken from `stak-decode` output instead.
- `mstak` takes no script path; integer-build experiments go through `stak-compile` and `mstak-interpret`.

## Appendix: reproduction

```sh
# batch compile and run on each build
cat prelude.scm prog.scm | target/release_test/stak-compile > prog.bc
target/release/stak-interpret prog.bc                 # float
cmd/minimal/target/release/mstak-interpret prog.bc    # 63-bit integer
target/release/stak-decode < prog.bc                  # disassemble
hyperfine -N --warmup 3 --runs 30 'target/release/stak-interpret prog.bc'
```
