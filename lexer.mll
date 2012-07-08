{
  open Ast
  open Lexing

  type lexer_ctx =
      {
        name : string;
        mutable line : int;
      }

  let mk_ctx name = { name = name; line = 1 }

  let cur_ctx = ref (mk_ctx "")

  type error_msg =
    | Invalid_char of char
    | Unterminated_string

  let error_msg = function
    | Invalid_char c -> "Syntax error on character: " ^ Char.escaped c
    | Unterminated_string -> "String does not terminate"

  exception Error of error_msg * pos

  let error e pos =
    raise (Error (e,{pmin = pos; pmax = pos; pname = !cur_ctx.name}))

  let invalid_char lexbuf =
    error (Invalid_char (lexeme_char lexbuf 0)) (lexeme_start lexbuf)

  let buf = Buffer.create 100
  let store lexbuf = Buffer.add_string buf (lexeme lexbuf)

  let newline =
    let ctx = !cur_ctx in
    ctx.line <- ctx.line + 1

  let mk_tok tk min max =
    tk , { pname = !cur_ctx.name; pmin = min; pmax = max }

  let mk lexbuf tk =
    mk_tok tk (lexeme_start lexbuf) (lexeme_end lexbuf)
}

let nl = "\r\n" | '\r' | '\n'
let ident = (['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '-' '0'-'9']*)

rule token = parse
  | eof { mk lexbuf Eof }
  | [' ' '\t']+ { token lexbuf }
  | nl { newline; token lexbuf }
  | ['0'-'9']+ { mk lexbuf (Const (Int (lexeme lexbuf))) }
  | ";" [^ '\r' '\n']* { mk lexbuf (Comment (lexeme lexbuf)) }
  | '(' { mk lexbuf LParen }
  | ')' { mk lexbuf RParen }
  | "'" { mk lexbuf Quote }
  | "\"" {
    Buffer.reset buf;
    let pmin = lexeme_start lexbuf in
    let pmax = (try string lexbuf with Exit -> error Unterminated_string pmin) in
    let str = Buffer.contents buf in
    mk_tok (Const (String str)) pmin pmax;
  }
  | ':' ident { let kwd = lexeme lexbuf in
                mk lexbuf (Const (Keyword (String.sub kwd 1
                                             (String.length kwd - 1))))
              }
  | ident { mk lexbuf (Const (Ident (lexeme lexbuf))) }
  | ['+' '-' '*' '/'] { mk lexbuf (Const (Ident (lexeme lexbuf))) }
  | _ { invalid_char lexbuf }

and string = parse
  | nl | eof { raise Exit }
  | '"' { lexeme_end lexbuf }
  | _ { store lexbuf; string lexbuf }
