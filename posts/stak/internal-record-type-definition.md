# Internal `define-record-type` in Scheme

> Internal definitions are not translated into `letrec*` because outputs can
> have internal `define-record-type`, which we don't know how to expand into
> internal (ordinary) definitions.

This is a statement in readme in the [`r7expander`][r7expander]
repository, which is one of the macro expanders for R7RS Scheme.

The R7RS standard is the report and practically the specification of the Scheme
programming language. In addition to its hygienic macro system inherited from
R5RS, it also provides a new syntax called `define-record-type` that allows
programs to define custom product types. The following example defines a custom
type named `foo`,

```scheme
(define-record-type foo
  (make-foo bar baz)
  foo?
  (bar foo-bar)
  (baz foo-baz foo-set-baz!))
```

where the `make-foo` is the constructor, `foo?` is the predicate, and then
getters of `foo-bar` and `foo-baz` and a setter of `foo-set-baz!` are defined.

I believe [Stak Scheme][stak]'s implementation is so called the generative one,
which [SRFI-9][srfi-9] describes. The type's identity changes every time the
internal definitions are evaluated while the non-generative version foo.

The [SRFI-9][srfi-9] document says:

> Record-type definitions may only occur at top-level (there are two possible semantics for 'internal' record-type definitions, generative and nongenerative, and no consensus as to which is better).

I hope this note saves some time from some Scheme implementors.

[r7expander]: https://github.com/nyuichi/r7expander
[stak]: https://github.com/raviqqe/stak
[srfi-9]: https://srfi.schemers.org/srfi-9/srfi-9.html
