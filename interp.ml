open Ast

type value =
    VNil
  | VBool of bool
  | VString of string
  | VSymbol of string
  | VKeyword of string
  | VInteger of int
  | VCallable of vcallable
  | VList of value list

and vcallable =
    VFunction of vfunction
  | VHash (* of TODO *)

and vfunction =
    Fun0 of (unit -> value)
  | Fun1 of (value -> value)
  | Fun2 of (value -> value -> value)
  | Fun3 of (value -> value -> value -> value)
  | Fun4 of (value -> value -> value -> value -> value)
  | FunVar of (value list -> value)

let args_for_vfunction = function
  | Fun0 _ -> 0
  | Fun1 _ -> 1
  | Fun2 _ -> 2
  | Fun3 _ -> 3
  | Fun4 _ -> 4
  | FunVar _ -> -1

let rec str_value = function
  | VNil -> "nil"
  | VBool b -> string_of_bool b
  | VString s -> s
  | VSymbol s -> s
  | VKeyword k -> ":" ^ k
  | VInteger i -> string_of_int i
  | VCallable c -> (match c with VFunction _ -> "<callable:func>" | VHash -> "<callable:hash>")
  | VList l -> "(" ^ (String.concat " " (List.map str_value l)) ^ ")"

let str_value_type = function
  | VNil -> "nil"
  | VBool _ -> "bool"
  | VString _ -> "string"
  | VSymbol _ -> "symbol"
  | VKeyword _ -> "keyword"
  | VInteger _ -> "integer"
  | VCallable _ -> "callable"
  | VList _ -> "list"

type context = {
  globals : (string, value) Hashtbl.t;
}

exception Builtin_error of string
exception Runtime_error of value

let runtime_error msg =
  raise (Runtime_error (VString msg))

let builtins =
  let error msg = raise (Builtin_error msg) in
  let vint = function
    | VInteger i -> i
    | t -> error (Printf.sprintf "Invalid type (got %s, expected integer)" (str_value_type t))
  in
  let literals = [
    "nil", VNil;
    "true", VBool true;
    "false", VBool false;
  ] in
  let funcs = [
    "+", FunVar (fun args -> VInteger (List.fold_left (fun acc x -> acc + (vint x)) 0 args));
    "*", FunVar (fun args -> VInteger (List.fold_left (fun acc x -> acc * (vint x)) 1 args));
    "-", FunVar (fun args ->
      (match args with
        | [] -> VInteger 0
        | hd :: [] -> VInteger (- vint hd)
        | hd :: tl -> VInteger (List.fold_left (fun acc x -> acc - (vint x)) (vint hd) tl)));
    "/", FunVar (fun args ->
      (* TODO: because of integer division, this is useless *)
      (match args with
        | [] -> runtime_error "/ needs at least 1 argument"
        | hd :: [] -> VInteger (1 / (vint hd))
        | hd :: tl -> VInteger (List.fold_left (fun acc x -> acc / (vint x)) (vint hd) tl)));

    (* TODO: this should be more thorough *)
    "eq", Fun2 (fun a b -> VBool (a = b));

    "print", Fun1 (fun v -> print_endline (str_value v); VNil);
    "str", FunVar (fun args -> VString (String.concat "" (List.map str_value args)))
  ] in
  let h = Hashtbl.create 20 in
  List.iter (fun (n,v) -> Hashtbl.add h n v) literals;
  List.iter (fun (n,f) -> Hashtbl.add h n (VCallable (VFunction f))) funcs;
  h

let make_context () =
  { globals = builtins }

let rec eval exp ctx =
  match exp with
    | EConst c ->
        (match c with
          | Int i -> VInteger (int_of_string i)
          | String s -> VString s
          | Keyword k -> VKeyword k
          | Ident i -> try Hashtbl.find ctx.globals i
                       with Not_found -> runtime_error "Can't find symbol")
    | ESexp s ->
        (match s with
          | [] -> VNil
          | fn :: [] -> call (eval fn ctx) [] ctx
          | fn :: args ->
              call
                (eval fn ctx)
                (List.map (fun a -> eval a ctx) args)
                ctx)
    | EQuoted q -> val_of_exp q

and call fn args ctx =
  match fn with
    | VCallable c ->
        (match c with
          | VFunction fnc ->
              (match fnc, args with
                | Fun0 f, [] -> f ()
                | Fun1 f, [a] -> f a
                | Fun2 f, [a;b] -> f a b
                | Fun3 f, [a;b;c] -> f a b c
                | Fun4 f, [a;b;c;d] -> f a b c d
                | FunVar f, _ -> f args
                | _ ->
                    runtime_error
                      (Printf.sprintf "Bad arguments given (got %d, expected %d)"
                         (List.length args)
                         (args_for_vfunction fnc)))
          | VHash _ -> VNil)
    | _ -> runtime_error "Invalid callable"

and val_of_exp exp =
  match exp with
    | EConst c ->  (match c with
          | Int i -> VInteger (int_of_string i)
          | String s -> VString s
          | Keyword k -> VKeyword k
          | Ident i -> VSymbol i)
    | ESexp s -> VList (List.map val_of_exp s)
        (* TODO: fix this *)
    | EQuoted q -> VList [(VSymbol "quote"); val_of_exp q]
