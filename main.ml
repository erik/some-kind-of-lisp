open Lexer
open Parser
open Ast
open Codegen
open Interp

open Arg

let error msg pos =
  Printf.printf "%s: %d-%d: %s " pos.pname pos.pmin pos.pmin msg;
  exit 1

let run lexbuf context =
  let exps = Parser.parse lexbuf in
  try
    let vals = List.map (fun exp -> eval exp context) exps in
    List.iter (fun v -> print_endline (str_value v)) vals
  with
    | Runtime_error v -> print_endline ("Runtime error: " ^ str_value v)
    | Builtin_error s -> print_endline ("Builtin function error: " ^ s)
    | Lexer.Error (e,p) -> error (Lexer.error_msg e) p
    | Parser.Error (e,p) -> error (Parser.error_msg e) p

let _ =
  let file = ref "" in
  let usage = "usage: " ^ Sys.argv.(0) ^ " [-r file]" in

  let argspec = [
    ("-r", Arg.String (fun f -> file := f), ": evaluate named file");
  ] in

  Arg.parse argspec (fun _ -> raise (Arg.Bad usage)) usage;

  let context = make_context () in

  if !file = "" then (* no file given, run repl *)
    try
      while true do
        print_string "> "; flush stdout;

        let line = read_line () in
        let lexbuf = Lexing.from_string line in
        run lexbuf context;
      done
    with End_of_file -> exit 0

  else
    let chan = open_in !file in
    let lexbuf = Lexing.from_channel chan in
    run lexbuf context
