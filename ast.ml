type pos = {
  pname : string;
  pmin : int;
  pmax : int;
}

type constant =
  | Int of string
  | String of string
  | Keyword of string
  | Ident of string

type token =
  | Eof
  | LParen
  | RParen
  | Quote
  | Backquote
  | Const of constant
  | Comment of string

type expr =
  | EConst of constant
  | ESexp of expr list
  | EQuoted of expr

let str_constant = function
  | Int c -> c
  | Keyword c -> ":" ^ c
  | String c -> "\"" ^ c ^ "\""
  | Ident c -> c

let str_token = function
  | Eof -> "<eof>"
  | LParen -> "("
  | RParen -> ")"
  | Const c -> str_constant c
  | Comment c -> ";" ^ c
  | Quote -> "'"
  | Backquote -> "`"

let rec str_expr = function
  | EQuoted e -> "`" ^ str_expr e
  | EConst c -> str_constant c
  | ESexp s -> str_sexp s

and str_sexp s =
  "(" ^ (String.concat " " (List.map (fun exp -> str_expr exp) s)) ^ ")"
