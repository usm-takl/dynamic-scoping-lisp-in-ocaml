%token <Sexp.t> DATUM
%token LPAREN RPAREN DOT QUOTE EOF
%type <Sexp.t list> program
%start program

%%

program:
| sexp* EOF { $1 }

sexp:
| DATUM { $1 }
| LPAREN list_body1 { $2 }
| QUOTE sexp { Sexp.Cons(Sexp.intern "quote", Sexp.Cons($2, Sexp.Nil)) }

list_body1:
| RPAREN { Sexp.Nil }
| sexp list_body2 { Sexp.Cons($1, $2) }

list_body2:
| DOT sexp RPAREN { $2 }
| RPAREN { Sexp.Nil }
| sexp list_body2 { Sexp.Cons($1, $2) }
