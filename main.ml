let () =
  let sexps = Parser.program Lexer.token (Lexing.from_channel stdin) in
  List.iter (fun sexp -> ignore @@ Sexp.eval sexp) sexps
