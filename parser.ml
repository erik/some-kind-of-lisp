open Ast

type error_msg =
  | Unexpected of token
  | Custom of string

exception Error of error_msg * pos

let error_msg = function
  | Unexpected t -> "Unexpected token: " ^ str_token t
  | Custom s -> s

let error e p =
  raise (Error (e,p))

let rec atom = parser
  | [< '(Const c, _) >] -> EConst c

and expr_list exps s =
  match s with parser
    | [< e = expr; l = expr_list (e :: exps) >] -> l
    | [< >] -> List.rev exps

and expr = parser
  | [< a = atom >] -> a
  | [< '(Quote, _); e = expr >] -> ESexp [(EConst (Ident "quote")); e]
  | [< '(LParen, _); e = expr_list []; '(RParen, _) ?? "expected closing paren" >] -> ESexp e

let parse lexbuf =
  let rec next_token () = parse_token (Lexer.token lexbuf)

  and parse_token tk =
    match fst tk with
      | Comment _ -> next_token ()
      | _ -> tk

  in
  let s = Stream.from (fun _ -> let tk = next_token () in Some tk) in
  try
    let rec exprs exps s =
      (match s with parser
        | [< e = expr; l = exprs (e :: exps) >] -> l
        | [< '(Eof, _) >] -> List.rev exps
        | [< '(tk, p) >] -> error (Unexpected tk) p) in
    exprs [] s
  with
    | Stream.Error _
    | Stream.Failure ->
        print_endline "Some kind of parse error";
        raise Stream.Failure
    | e ->
        Printexc.print_backtrace stdout;
        raise e
