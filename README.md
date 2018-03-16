# dynamic-scoping-lisp-in-ocaml

This is a toy lisp implementation, which adopts dynamic scoping.

# How to build

```
$ jbuilder build main.exe
```

or

```
$ menhir parser.mly
$ ocamllex lexer.mll
$ ocamlc sexp.ml parser.mli parser.ml lexer.ml main.ml
```

# How to run

This program receives a lisp program from stdin.

```
$ echo "(display '(hello world))" | _build/default/main.exe
```

or

```
$ echo "(display '(hello world))" | ./a.out
```

etc.
