let space = ['\t' '\n' '\r' ' ']
let symbol_char =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' '0'-'9' '<' '=' '>' '?'
   '@' 'A'-'Z' '^' '_' 'a'-'z' '~']

rule token = parse
| space+ { token lexbuf }
| symbol_char+ as lexeme {
    match float_of_string_opt lexeme with
    | Some f ->
        Parser.DATUM(Sexp.Number(f))
    | None when lexeme = "t" ->
        Parser.DATUM(Sexp.T)
    | None when lexeme = "." ->
        Parser.DOT
    | None ->
        Parser.DATUM(Sexp.intern(lexeme))
  }
| "#|" { block_comment lexbuf; token lexbuf }
| ";" { line_comment lexbuf; token lexbuf }
| '\'' { Parser.QUOTE }
| '(' { Parser.LPAREN }
| ')' { Parser.RPAREN }
| eof { Parser.EOF }
| '"' (([^'"''\\'] | '\\'['\x00'-'\xff'])+ as lexeme) '"' {
    Parser.DATUM(Sexp.String(Scanf.unescaped lexeme))
  }

and line_comment = parse
| ('\n' | eof) { () }
| _ { line_comment lexbuf }

and block_comment = parse
| "|#" { () }
| "#|" { block_comment lexbuf; block_comment lexbuf }
| eof { () }
| _ { block_comment lexbuf }
