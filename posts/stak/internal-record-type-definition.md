# Internal `define-record-type` in Scheme

This is the article on December 8th for [the Qiita Advent Calendar 2025 of
language implementation](https://qiita.com/advent-calendar/2025/lang_dev).

## Nontrivial expansion

> Internal definitions are not translated into `letrec*` because outputs can
> have internal `define-record-type`, which we don't know how to expand into
> internal (ordinary) definitions.

This is a note in readme of the [`r7expander`][r7expander]
project, which is one of the macro expanders for R7RS Scheme.

The R7RS standard is the report and practically the 8th specification of the
Scheme programming language.
In addition to its hygienic macro system inherited from R5RS, it also provides
a new syntax called `define-record-type` that allows
Scheme programs to define custom data types. The following example defines a
custom data type named `foo`,

```scheme
(define-record-type foo
  (make-foo bar baz)
  foo?
  (bar foo-bar)
  (baz foo-baz foo-set-baz!))
```

where the `make-foo` is the constructor, `foo?` is the predicate, and then
getters of `foo-bar` and `foo-baz` and a setter of `foo-set-baz!` follow.

Although it is obvious how to expand it into global definitions, it is not so
obvious how to interpret them as internal definitions that appear within the
`lambda` bodies.

```scheme
(lambda ()
  (define-record-type foo
    (make-foo bar baz)
    foo?
    (bar foo-bar)
    (baz foo-baz foo-set-baz!))
  ; ...
```

While I was working on [my own Scheme implementation in Rust][stak], where all
the syntaxes in R7RS are implemented in Scheme itself via the macro system,
I also had some difficulty to figure out the cleanest way to implement
such internal record type definitions without introducing any ad-hoc compiler
logic or new primitives.

## Implementation

The following is the sample implementation of internal `define-record-type` with
pure `define-syntax` and `syntax-rules` in Scheme. This is pretty much the same
as the implementation in [Stak Scheme][stak] although it uses lists for the
internal representation of record fields. Also, it skips most of input
validation for procedures and syntaxes.

```scheme
(define-syntax define-record-type
  (syntax-rules ()
    ((_ "initial"
        id
        constructor
        predicate
        accessors
        body
        ...)
      (define-record-type
        "field"
        (body ...)
        accessors
        0
        ; Generate a type "ID" by making a unique referential data.
        (define id (cons 0 0))
        (define constructor (record-constructor id))
        (define predicate (record-predicate id))))

    ((_ "field" bodies ((get set ...) accessor ...) index statement ...)
      (define-record-type
        "field"
        bodies
        (accessor ...)
        (+ index 1)
        statement
        ...
        (define get (record-getter index))
        ; Zero or more is equivalent to zero or one if there is only up to one.
        (define set (record-setter index))
        ...))

    ((_ "field" () () _ statement ...)
      (begin statement ...))

    ((_ "field" (body ...) () _ statement ...)
      (let () statement ... body ...))

    ((_ id
        (constructor _ ...)
        predicate
        (_ accessor ...)
        ...)
      (define-record-type
        "initial"
        id
        constructor
        predicate
        ((accessor ...) ...)))))

(define-syntax let-record-type
  (syntax-rules ()
    ((_ (id
          (constructor _ ...)
          predicate
          (_ accessor ...)
          ...)
        body
        ...)
      (define-record-type
        "initial"
        id
        constructor
        predicate
        ((accessor ...) ...)
        body
        ...))))

(define record? (instance? record-type))

(define (record-constructor id)
  (lambda fields
    (##make-record id fields)))

(define (record-predicate id)
  (lambda (x)
    (and
      (##record? x)
      (eq? (##record-id x) id))))

(define (record-getter index)
  (lambda (record)
    (vector-ref (##record-fields record) index)))

(define (record-setter index)
  (lambda (record value)
    (vector-set! (##record-fields record) index value)))
```

The record type primitives are marked with `##` prefixes to the symbols.

There are several techniques here:

- The syntax for internal definitions (i.e. `let-record-type`)
  enters an intermediate state of the `define-record-type` expansion.
  This allows us to share the syntax rules between the global and internal
  definitions.
- We expand accessors of a record step by step. Eventually, we expand
  all lowered definitions into global or internal definitions depending on the
  original syntax.
- We generate the unique record type IDs by creating a new `cons` every time the
  definition is evaluated.

The `let-record-type` defines an internal record type definition.
It can be used in a typical expansion of `lambda` bodies with internal
definitions like other `let-*` variants.

```scheme
(define-syntax lambda
  (syntax-rules (define define-syntax define-record-type define-values)
    ; ...

    ((_ arguments (define-record-type item ...) body1 body2 ...)
      (lambda arguments (let-record-type (item ...) body1 body2 ...)))
```

I believe this implementation in [Stak Scheme][stak] is so called the generative
one that [SRFI-9][srfi-9] describes.
The type's identity changes every time the
internal definitions are evaluated, while in the nongenerative version,
the type's identity remains the same across multiple evaluations of the
definition.

The [SRFI-9][srfi-9] document says:

> Record-type definitions may only occur at top-level (there are two possible semantics for 'internal' record-type definitions, generative and nongenerative, and no consensus as to which is better).

I hope this note saves some time from some Scheme implementors.

[r7expander]: https://github.com/nyuichi/r7expander
[stak]: https://github.com/raviqqe/stak
[srfi-9]: https://srfi.schemers.org/srfi-9/srfi-9.html
