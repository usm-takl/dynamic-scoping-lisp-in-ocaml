type t =
  | Nil
  | T
  | Symbol of symbol
  | String of string
  | Number of float
  | Cons of t * t
  | Subr of (t -> t)
  | Core of (t -> t)
  | Procedure of t * t (* params, body *)
and symbol = { symbol_name : string; mutable symbol_values : t list }

exception Error of string

let obarray = Hashtbl.create 256

let intern symbol_name =
  if Hashtbl.mem obarray symbol_name then
    Hashtbl.find obarray symbol_name
  else
    let symbol = Symbol {symbol_name; symbol_values = [Nil]} in
    Hashtbl.add obarray symbol_name symbol;
    symbol

let setq symbol value =
  match symbol with
  | Symbol s -> s.symbol_values <- value :: List.tl s.symbol_values
  | _ -> raise (Error "setq")

let push symbol value =
  match symbol with
  | Symbol s -> s.symbol_values <- value :: s.symbol_values
  | _ -> raise (Error "push")

let pop symbol =
  match symbol with
  | Symbol s -> s.symbol_values <- List.tl s.symbol_values
  | _ -> raise (Error "pop")

let rec display = function
  | Nil ->
     print_string "()"
  | T ->
     print_string "t"
  | Symbol { symbol_name } ->
     print_string symbol_name
  | String s ->
     print_string s
  | Number f ->
     print_float f
  | Cons(car, cdr) ->
     print_string "(";
     display car;
     print_list cdr
  | Subr _ ->
     print_string "#<subr>"
  | Core _ ->
     print_string "#<core>"
  | Procedure _ ->
     print_string "#<procedure>"
and print_list = function
  | Nil ->
     print_char ')'
  | Cons(car, cdr) ->
     print_char ' ';
     display car;
     print_list cdr
  | cdr ->
     print_string " . ";
     display cdr;
     print_char ')'

let car = function
  | Nil -> Nil
  | Cons(car, cdr) -> car
  | _ -> raise (Error "car")

let cdr = function
  | Nil -> Nil
  | Cons(car, cdr) -> cdr
  | _ -> raise (Error "cdr")

let cadr list = car (cdr list)

let cddr list = cdr (cdr list)

let caddr list = car (cddr list)

let rec map fn = function
  | Nil -> Nil
  | Cons(car, cdr) -> Cons(fn car, map fn cdr)
  | _ -> raise (Error "map")

let rec iter fn = function
  | Nil -> ()
  | Cons(car, cdr) -> fn car; iter fn cdr
  | _ -> raise (Error "iter")

let rec iter2 fn l1 l2 =
  match l1, l2 with
  | Nil, Nil -> ()
  | Cons(car1, cdr1), Cons(car2, cdr2) -> fn car1 car2; iter2 fn cdr1 cdr2
  | _ -> raise (Error "iter2")

let rec eval expr =
  match expr with
  | Nil -> expr
  | T -> expr
  | Symbol {symbol_values} -> List.hd symbol_values
  | String _ -> expr
  | Number _ -> expr
  | Procedure _ -> expr
  | Subr _ -> expr
  | Core _ -> expr
  | Cons(op, args) ->
     match eval op with
     | Procedure(params, body) ->
        iter2 (fun p a -> push p (eval a)) params args;
        let ret = progn body in
        iter pop params;
        ret
     | Subr fn -> fn (map eval args)
     | Core fn -> fn args
     | _ -> raise (Error "not an operator")
and progn = function
  | Nil -> Nil
  | Cons(car, Nil) -> eval car
  | Cons(car, cdr) -> ignore @@ eval car; progn cdr
  | _ -> raise (Error "progn")

let subr_display =
  Subr (function
      | Cons(arg, Nil) -> display arg; Nil
      | _ -> raise (Error "subr_display: wrong number of arguments"))

let subr_add =
  Subr(function
      | Cons(Number a, Cons(Number b, Nil)) -> Number(a +. b)
      | _ -> raise (Error "add: invalid arguments"))

let subr_sub =
  Subr(function
      | Cons(Number a, Cons(Number b, Nil)) -> Number(a -. b)
      | _ -> raise (Error "sub: invalid arguments"))

let subr_mul =
  Subr(function
      | Cons(Number a, Cons(Number b, Nil)) -> Number(a *. b)
      | _ -> raise (Error "mul: invalid arguments"))

let subr_div =
  Subr(function
      | Cons(Number a, Cons(Number b, Nil)) -> Number(a /. b)
      | _ -> raise (Error "div: invalid arguments"))

let subr_car =
  Subr(function
      | Cons(Cons(car, _), Nil) -> car
      | _ -> raise (Error "subr_car"))

let subr_cdr =
  Subr(function
      | Cons(Cons(_, cdr), Nil) -> cdr
      | _ -> raise (Error "subr_cdr"))

let subr_cons =
  Subr(function
      | Cons(car, Cons(cdr, Nil)) -> Cons(car, cdr)
      | _ -> raise (Error "subr_cons: wrong number of arguments"))

let subr_not =
  Subr(function
      | Cons(Nil, Nil) -> T
      | Cons(_, Nil) -> Nil
      | _ -> raise (Error "subr_not: invalid arguments"))

let subr_equal =
  Subr(function
      | Cons(car, Cons(cadr, Nil)) -> if car = cadr then T else Nil
      | _ -> raise (Error "subr_equal: wrong number of arguments"))

let core_assert =
  Core(function
      | Cons(expr, Nil) ->
         if eval expr = T then
           Nil
         else (
           display expr;
           raise (Error "assertion failed")
         )
      | _ ->
         raise (Error "malformed assert expression"))

let core_quote =
  Core(function
      | Cons(car, Nil) -> car
      | _ -> raise (Error "malformed quote expression"))

let core_if =
  Core(fun args ->
      if eval (car args) <> Nil then
        eval (cadr args)
      else if cddr args <> Nil then
        eval (caddr args)
      else
        Nil)

let core_setq =
  Core(function
      | Cons(symbol, Cons (value, Nil)) ->
         setq symbol (eval value); Nil
      | _ ->raise (Error "malformed setq expression"))

let core_defun =
  Core(function
      | Cons(symbol, Cons (params, body)) ->
         setq symbol @@ Procedure(params, body); Nil
      | _ -> raise (Error "malformed defun expression"))

let core_lambda =
  Core(function
      | Cons(args, body) -> Procedure(args, body)
      | _ -> raise (Error "malformed lambda expression"))

let core_let =
  let init1 = function
    | Cons(car, Cons(cadr, Nil)) ->
       push car (eval cadr)
    | Cons(car, Nil) ->
       ()
    | _ -> raise (Error "malformed let expression")
  in
  let rec init = function
    | Nil -> ()
    | Cons(car, cdr) -> init1 car; init cdr
    | _ -> raise (Error "malformed let expression")
  in
  let rec fini = function
    | Nil -> ()
    | Cons(Cons(var, _), cdr) -> pop var; fini cdr
    | _ -> raise (Error "malformed let expression")
  in
  Core(function
      | Cons(car, cdr) ->
         init car;
         let tmp = progn cdr in
         fini car;
         tmp
      | _ -> raise (Error "malformed quote expression"))

;;
setq (intern "display") subr_display;
setq (intern "+") subr_add;
setq (intern "-") subr_sub;
setq (intern "*") subr_mul;
setq (intern "/") subr_div;
setq (intern "=") subr_equal;
setq (intern "car") subr_car;
setq (intern "cdr") subr_cdr;
setq (intern "cons") subr_cons;
setq (intern "not") subr_not;
setq (intern "assert") core_assert;
setq (intern "quote") core_quote;
setq (intern "if") core_if;
setq (intern "setq") core_setq;
setq (intern "defun") core_defun;
setq (intern "lambda") core_lambda;
setq (intern "let") core_let;
