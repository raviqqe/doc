# Inception: The self-embedding compiler in Stak Scheme

![A spinning top](https://raviqqe.s3.amazonaws.com/doc/stak/inception/spinning_top_3.jpg)

Code duplication is one of the primary sources of bugs and maintenance burden in software engineering. If a language processor itself is its target codes, what can we do?

In the last few years, I've been developing a Scheme interpreter in Rust called [Stak Scheme][stak].
The Scheme interpreter aims for small memory footprints with [reasonable performance](https://github.com/raviqqe/stak?tab=readme-ov-file#performance) and to be a complete implementation of the [R7RS-small][r7rs-small] standard.
While I implemented it originally as a re-implementation of [Ribbit Scheme][ribbit], I was wondering how to avoid code duplication of its compiler and the `eval` library. This article depicts how Stak Scheme's bytecode compiler "incepts" itself into the `eval` procedure in its target codes.

## TL;DR

- Handling a compiler itself as data in the compiler itself allows it to be embedded into target codes.
- S-expressions and its homoiconicity is stunning. Lisp all the things :)

## Code duplication between the compiler and the `eval` library

First things first, Stak Scheme (and Ribbit Scheme) has an irregular architecture of the whole language processor; it is composed of a bytecode compiler written in Scheme and a virtual machine (VM) in Rust. The benefit of such an architecture is that we can omit a big chunk of logic for language processing from resulting artifacts, such as executable binaries. Because macro expansion, optimization, and actual compilation down to bytecodes are all inside the compiler split from the VM, we do not have to put the extra bytes for their logic into the artifacts. I believe [this is a key decision made by the research team of Ribbit Scheme](https://www.iro.umontreal.ca/~feeley/papers/YvonFeeleyVMIL21.pdf) for the goal of small memory footprints.

Although the architecture is beneficial for stripping down extra bytes from artifacts, it comes with some pain points.
The primary one is duplication of logic compiling S-expressions into bytecodes.
In R7RS, you define libraries using the `define-library` syntax. This also allows us to strip unnecessary codes for the `eval` library (i.e. the `(scheme eval)` library in R7RS) in most cases unless we import the library explicitly with the `import` syntax. In the `eval` procedure, we need to compile a given S-expression into bytecodes and execute it to evaluate the expression. The compilation part of the procedure is exactly identical to what the bytecode compiler does. Hence, Stak Scheme originally had two separate implementation of such compilation logic in two Scheme files; one is the `compile.scm` compiler script and the other is the `prelude.scm` file that contains all the libraries including the `(scheme eval)` one.

## "Incepting" a compiler into its target codes

The basic idea to resolve the code duplication problem between the bytecode compiler and the `eval` library is simply to copy the compiler into source codes it is compiling. In languages of Lisp variants, this is a lot easier than the other languages as S-expressions are representation of both codes and data. In the case of Stak Scheme, the former is the compiler and the latter is source codes given to it.

The basic structure of the new `eval` procedure looks like the following.

```scheme
(define eval
  (let ((compile ($$compiler)))
    (lambda (expression environment)
      ((compile expression environment)))))
```

You see the `($$compiler)` primitive call in the middle of the function body. The bytecode compiler replaces the primitive call with source codes of the compiler in S-expressions right after reading source codes. The reason it is not just a `$$compiler` symbol but wrapped with parentheses making it a form of a procedure/macro call is that we cannot differentiate such a symbol from normal uses of the symbol; the compiler itself contains the `$$compiler` symbol to replace it in source codes!

### Compiler frontend and backend

Note that the previous `($$compiler)` directive is not replaced with the whole compiler in the `compile.scm` script. The script also contains the other logic, such as source code reading, expansion of library definitions, serialization of bytecodes, etc. Therefore, we first need to extract the "frontend" part of the compiler as data in S-expressions which looks like the following snippet.

```scheme
(define frontend
  '(
    ; Macro system

    ;; Types

    (define-record-type macro-state
     (make-macro-state id literals static-symbols dynamic-symbols)
     macro-state?
    ; ...

    ; Compilation

    ;; Context

    (define-record-type compilation-context
     (make-compilation-context environment metadata)
    ; ...
    ))
```

The compiler frontend contains all parts that the `eval` procedure need including macro system, optimization, and bytecode compilation. We instantiate the frontend of an S-expression into concrete codes in both the body of the compiler script itself and the `eval` procedure in target codes.

So now, we can embed the compiler frontend into the given source codes when we find any `($$compiler)` directive inside them.

```scheme
; The given expression is source codes which is already parsed into an S-expression.
(define (incept expression)
  (cond
    ((not (pair? expression))
      expression)
    ; Find a `($$compiler)` directive call in source codes.
    ((and
        (pair? (car expression))
        (null? (cdar expression))
        (eq? (caar expression) '$$compiler))
      (cons
        `(let ()
          ,@frontend

          ; ...

          ; Return a `eval` procedure which compiles an S-expression into bytecodes
          ; and executes it as a thunk (a procedure with no parameter.)
          (lambda (expression environment)
            (make-procedure
              (compile-arity 0 #f)
              (compile
                (optimize
                  (expand-macros
                    (expand-libraries environment expression))))
              '())))
        (cdr expression)))
     ; ...
     ))
```

The code is basically finding the `($$compiler)` directive call and replacing it with the expression that forms the `eval` procedure. In Lisp dialects, this is easier than in the other languages where we need to make the substitution in source codes of raw strings or define Abstract Syntax Tree (AST) for ourselves whereas in Lisp dialects both codes and data are represented in the same syntax, S-expressions.

On the other hand, the compiler itself looks like the following:

```scheme
(define compiler
  `(let ()
    ; Combine compiler frontend and backend.
    ,@frontend
    ,@backend))

; Evaluate the `compiler` expression into a `compile` procedure.
(define compile
  (eval
    compiler
    (environment
      '(scheme base)
      ; ...
      )))

; Read source codes, incept the compiler frontend, and finally compile it into bytecodes.
(compile (incept (read-source)))
```

## The other solutions

While you are reading to this point, you have probably thought of many other solutions. Indeed, I believe this is not the simplest solution for removing code duplication of the compiler logic although I do this is the best. The following sections describe the other solutions that didn't work well with Stak Scheme but might do for the other projets of language processors.

### Modularizing a compiler

Why don't we simply create a common module of the compiler logic in a separate file and share it between the compiler script and the `eval` library? Although this is a simple solution by itself, Stak Scheme did not take this path to keep the deployability of the compiler and libraries. Stak Scheme's compiler and the R7RS library set are contained within single files of `compiler.scm` and `prelude.scm` respectively. That makes the building and packaging of the compiler and libraries simple. So I didn't want to break it for the sake of less code duplication.

### Compiler as a library

Another option is providing a compiler as a library specifically inside one of standard libraries, such as `(scheme eval)`, in the `prelude.scm` file and a compiler simply calls the `compile` procedure in the library. However, this approach is problematic in the case we want to modify the bytecode format because Stak Scheme is self-hosted; technically the `compile.scm` script must be able to be run with any version of Stak Scheme. If the host side of Stak Scheme uses the old version of bytecodes but we change the new bytecode format of VM in Rust, we would break the system. Also, it makes the compiler dependent on Stak Scheme itself and we will not be able to use the other implementations of R7RS Scheme as a compiler host for [bootstrapping](<https://en.wikipedia.org/wiki/Bootstrapping_(compilers)>) anymore. When a compiler is embedding itself, we don't need to care about such incompatibility between the compiler, VM, and the host implementation of Scheme.

### Concatenating bytecodes

It is technically possible to compile source codes of the compiler and target codes separately and concatenate them together into a single chunk of target codes. However, this approach has a several downsides.

First, the bytecode compiler of Stak Scheme (or Ribbit Scheme) does not have any easy way to concatenate bytecodes as byte sequences. Because the bytecode compiler compiles source codes into bytecodes in its in-memory format of Directed Acyclic Graph (DAG) and then encodes them into its serialized format, bytecode concatenation is not as simple as concatenation of byte arrays. In Stak Scheme, bytecodes are not simple sequences of instructions but they represent codes and data in a more natural way. For example, conditional jumps are nodes connected with two outgoing edges leading to nodes of next instructions for its true and false cases.

Secondly, if the bytecode format changes we would need to change the way to seam the two chunks of bytecodes together, which is another maintenance burden.

## Results

I've found a piece of performance improvement that I did not introduce on the side of the `eval` library while it was already implemented in the compiler side. Also, as I stated earlier, we do not need to maintain two different implementations of the compiler. That does not only alleviate the development cost of maintaining the compiler but also accelerates its development because we can easily validate that new compiler changes in both the compiler script and the `eval` library.

## References

- [Stak Scheme][stak]
- [Ribbit Scheme - A portable, compact and extensible Scheme implementation that is fully R4RS compliant.][ribbit]

[stak]: https://github.com/raviqqe/stak
[ribbit]: https://github.com/udem-dlteam/ribbit
[r7rs-small]: https://small.r7rs.org/
