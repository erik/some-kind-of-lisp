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
    VFunction of vfunction * function_type
  | VHash (* of TODO *)

and vfunction =
    Fun0 of (context -> value)
  | Fun1 of (context -> value -> value)
  | Fun2 of (context -> value -> value -> value)
  | Fun3 of (context -> value -> value -> value -> value)
  | Fun4 of (context -> value -> value -> value -> value -> value)
  | FunVar of (context -> value list -> value)

and function_type =
  | Function
  | Macro

and context = {
  globals : (string, value) Hashtbl.t;
}

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
  | VCallable c -> (match c with
      | VFunction _ -> "<callable:func>"
      | VHash -> "<callable:hash>")
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

let bool_of_value = function
  | VNil -> VBool false
  | VBool b when b = false -> VBool false
  | _ -> VBool true

exception Builtin_error of string
exception Runtime_error of value

let runtime_error msg =
  raise (Runtime_error (VString msg))

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
          | fn :: args -> call (eval fn ctx) (List.map val_of_exp args) ctx)
    | EQuoted _ -> VNil

and call fn exps ctx =
  match fn with
    | VCallable c ->
        (match c with
          | VHash _ -> VNil
          | VFunction (fnc,t) ->
              let args =
                (match t with
                  | Function -> List.map (fun e -> unquote e ctx) exps
                  | Macro ->  exps)
              in
              match fnc, args with
                | Fun0 f, [] -> f ctx
                | Fun1 f, [a] -> f ctx a
                | Fun2 f, [a;b] -> f ctx a b
                | Fun3 f, [a;b;c] -> f ctx a b c
                | Fun4 f, [a;b;c;d] -> f ctx a b c d
                | FunVar f, _ -> f ctx args
                | _ ->
                    runtime_error
                      (Printf.sprintf "Bad arguments given (got %d, expected %d)"
                         (List.length args)
                         (args_for_vfunction fnc)))
    | _ -> runtime_error "Invalid callable"

and unquote v ctx = match v with
  | VSymbol s ->
      (try Hashtbl.find ctx.globals s
       with Not_found -> runtime_error ("Symbol " ^ s ^ " could not be resolved"))
  | VList l when l = [] -> VNil
  | VList l ->
      call (unquote (List.hd l) ctx) (List.tl l) ctx
  | v -> v

and val_of_exp exp =
  match exp with
    | EConst c ->  (match c with
        | Int i -> VInteger (int_of_string i)
        | String s -> VString s
        | Keyword k -> VKeyword k
        | Ident i -> VSymbol i)
    | ESexp s -> VList (List.map val_of_exp s)
    | EQuoted _ -> VNil

let builtins =
  let error msg = raise (Builtin_error msg) in
  let vint = function
    | VInteger i -> i
    | t -> error (Printf.sprintf "Invalid type (got %s, expected integer)" (str_value_type t))
  in
  let vsymbol = function
    | VSymbol s -> s
    | t -> error (Printf.sprintf "Invalid type (got %s, expected symbol)" (str_value_type t))
  in
  let vlist = function
    | VList l -> l
    | t -> error (Printf.sprintf "Invalid type (got %s, expected list)" (str_value_type t))
  in
  let literals = [
    "nil", VNil;
    "true", VBool true;
    "false", VBool false;
  ] in
  let funcs = [
    "+", FunVar (fun _ args -> VInteger (List.fold_left (fun acc x -> acc + (vint x)) 0 args));
    "*", FunVar (fun _ args -> VInteger (List.fold_left (fun acc x -> acc * (vint x)) 1 args));
    "-", FunVar (fun _ args ->
      (match args with
        | [] -> VInteger 0
        | hd :: [] -> VInteger (- vint hd)
        | hd :: tl -> VInteger (List.fold_left (fun acc x -> acc - (vint x)) (vint hd) tl)));
    "/", FunVar (fun _ args ->
      (* TODO: because of integer division, this is useless *)
      (match args with
        | [] -> runtime_error "/ needs at least 1 argument"
        | hd :: [] -> VInteger (1 / (vint hd))
        | hd :: tl -> VInteger (List.fold_left (fun acc x -> acc / (vint x)) (vint hd) tl)));

    (* TODO: this should be more thorough *)
    "eq", Fun2 (fun _ a b -> VBool (a = b));
    "print", Fun1 (fun _ v -> print_endline (str_value v); VNil);
    "str", FunVar (fun _ args -> VString (String.concat "" (List.map str_value args)));
    "do", FunVar (fun ctx args ->
      let rec loop exps =
        match exps with
          | [] -> VNil
          | hd :: [] -> unquote hd ctx
          | hd :: tl -> let _ = unquote hd ctx in loop tl
      in
      loop args);
  ] in
  let macros = [
    "fn", Fun2 (fun _ a b ->
      let args = vlist a in
      let fn = (match args with
        | [] -> Fun0 (fun c -> unquote b c)
        | _ -> runtime_error "Functions with arguments not yet supported (so useful, I know)")
      in
      VCallable (VFunction (fn, Function)));
    "quote", Fun1 (fun _ v -> v);
    "cond", FunVar (fun ctx args ->
      let rec do_cond conds =
        match conds with
          | [] -> VNil
          | c::b::rest ->
              if (bool_of_value (unquote c ctx)) = (VBool true) then (unquote b ctx)
              else do_cond rest
          | _ -> runtime_error "uneven number of conditions given to cond"
      in
      do_cond args);
    "def", Fun2 (fun ctx n d -> let name = vsymbol n in
                                let def = (unquote d ctx) in
                                Hashtbl.add ctx.globals name def;
                                def)
  ] in
  let h = Hashtbl.create 20 in
  List.iter (fun (n,v) -> Hashtbl.add h n v) literals;
  List.iter (fun (n,f) -> Hashtbl.add h n (VCallable (VFunction (f, Function)))) funcs;
  List.iter (fun (n,f) -> Hashtbl.add h n (VCallable (VFunction (f, Macro)))) macros;
  h

let make_context () =
  { globals = builtins }
