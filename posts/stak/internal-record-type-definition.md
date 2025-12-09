# Internal `define-record-type` in Scheme

> Internal definitions are not translated into `letrec*` because outputs can
> have internal `define-record-type`, which we don't know how to expand into
> internal (ordinary) definitions.

This is a note in readme of the [`r7expander`][r7expander]
repository, which is one of the macro expanders for R7RS Scheme.

The R7RS standard is the report and practically the specification of the Scheme
programming language. In addition to its hygienic macro system inherited from
R5RS, it also provides a new syntax called `define-record-type` that allows
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
getters of `foo-bar` and `foo-baz` and a setter of `foo-set-baz!` are defined.

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
the syntaxes in R7RS are implemented in Scheme itself, I also had some
difficulty to figure out the cleanest way to implement such internal record type
definitions without introducing any ad-hoc compiler logic.

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
  (lambda xs
    (data-rib record-type id xs)))

(define (record-predicate id)
  (lambda (x)
    (and
      (record? x)
      (eq? (car x) id))))

(define (record-getter index)
  (lambda (record)
    (list-ref (cdr record) index)))

(define (record-setter index)
  (lambda (record value)
    (list-set! (cdr record) index value)))
```

I believe [Stak Scheme][stak]'s implementation is so called the generative one,
which [SRFI-9][srfi-9] describes. The type's identity changes every time the
internal definitions are evaluated while the non-generative version foo.

The [SRFI-9][srfi-9] document says:

> Record-type definitions may only occur at top-level (there are two possible semantics for 'internal' record-type definitions, generative and nongenerative, and no consensus as to which is better).

I hope this note saves some time from some Scheme implementors.

[r7expander]: https://github.com/nyuichi/r7expander
[stak]: https://github.com/raviqqe/stak
[srfi-9]: https://srfi.schemers.org/srfi-9/srfi-9.html
