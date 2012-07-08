open Lexer
open Parser
open Ast
open Codegen
open Interp

let error msg pos =
  Printf.printf "%s: %d-%d: %s " pos.pname pos.pmin pos.pmin msg;
  exit 1

let _ =
  try
    let context = make_context () in
    while true do
      print_string "> "; flush stdout;

      let line = read_line () in
      let lexbuf = Lexing.from_string line in
      let exps = Parser.parse lexbuf in
      try
        let vals = List.map (fun exp -> eval exp context) exps in
        List.iter (fun v -> print_endline (str_value v)) vals
      with
        | Runtime_error v -> print_endline ("Runtime error: " ^ str_value v)
        | Builtin_error s -> print_endline ("Builtin function error: " ^ s)
    done
  with
    | Lexer.Error (e,p) -> error (Lexer.error_msg e) p
    | Parser.Error (e,p) -> error (Parser.error_msg e) p
    | End_of_file -> exit 0
