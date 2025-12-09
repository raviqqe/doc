# Internal `define-record-type` in Scheme

> Internal definitions are not translated into `letrec*` because outputs can
> have internal `define-record-type`, which we don't know how to expand into
> internal (ordinary) definitions.

This is a statement in a readme file in the [`r7expander`][r7expander]
repository, which is one of the macro expander for R7RS Scheme.

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

[Stak Scheme][stak]'s implementation is generative.

[r7expander]: https://github.com/nyuichi/r7expander
[stak]: https://github.com/raviqqe/stak
[srfi-9]: https://srfi.schemers.org/srfi-9/srfi-9.html
