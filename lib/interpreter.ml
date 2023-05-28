open Ast
open Env

(* Exceptions *)
exception UnboundException of string
exception DuplicateException of string
exception SecurityException of string

(* Indicates the trust level of the code being evaluated *)
type trust_level = High | Normal | Low

(** Turns the value [e] into a string.
    Requires: [e] is a value. *)
let string_of_val (e : value) : string =
  match e with
  | IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | StringV s -> s
  | Closure _ -> "[Closure]"
  | UntrustedCode _ -> "[Untrusted Code]"

(** Returns the value computed by evaluating [v1 bop v2]. *)
let eval_bop (bop : op) (v1 : value) (v2 : value) : value =
  match (bop, v1, v2) with
  | Add, IntV a, IntV b -> IntV (a + b)
  | Sub, IntV a, IntV b -> IntV (a - b)
  | Mult, IntV a, IntV b -> IntV (a * b)
  | Div, IntV a, IntV b ->
      if b <> 0 then IntV (a / b) else failwith "Division by 0."
  | Eq, IntV a, IntV b -> BoolV (a == b)
  | Eq, BoolV a, BoolV b -> BoolV (a == b)
  | Eq, StringV a, StringV b -> BoolV (a = b)
  | Gt, IntV a, IntV b -> BoolV (a > b)
  | Gte, IntV a, IntV b -> BoolV (a >= b)
  | Lt, IntV a, IntV b -> BoolV (a < b)
  | Lte, IntV a, IntV b -> BoolV (a <= b)
  | And, BoolV a, BoolV b -> BoolV (a && b)
  | Or, BoolV a, BoolV b -> BoolV (a || b)
  | _ -> failwith "Unrecognized operation."

(** Returns the value obtained by negating [e]. *)
let eval_not (e : value) : value =
  match e with BoolV b -> BoolV (not b) | _ -> failwith "Not a boolean."

(** Returns the value obtained by evaluating [e] in [environment] with trust level [trust]. *)
let rec eval (e : expr) (environment : env) (trust : trust_level) : value =
  match e with
  (* Integers, booleans and strings are already values. *)
  | Int i -> IntV i
  | Bool b -> BoolV b
  | String s -> StringV s
  (* Variables require a lookup *)
  | Var x -> (
      try Env.find x environment
      with Not_found -> raise (UnboundException ("Unbound symbol: " ^ x)))
  (* Evaluate binary expressions by first evaluating the operands. *)
  | Binop (bop, e1, e2) ->
      eval_bop bop (eval e1 environment trust) (eval e2 environment trust)
  (* The "not" is only applied on a fully evaluated boolean expression. *)
  | Not e -> eval_not (eval e environment trust)
  (* First evaluate the guard, then the appropriate branch *)
  | IfThenElse (guard, then_branch, else_branch) -> (
      match eval guard environment trust with
      | BoolV b ->
          if b then eval then_branch environment trust
          else eval else_branch environment trust
      | _ -> failwith "Invalid guard.")
  (* Update the environment with a new assignment *)
  | Let (id, body, rest) ->
      let v = eval body environment trust in
      let new_env = Env.add id v environment in
      eval rest new_env trust
  (* Return a closure of the appropriate security level *)
  | Fun (arg_id, body) -> (
      match trust with
      | High -> Closure (arg_id, body, environment, Enclaved)
      | _ -> Closure (arg_id, body, environment, Regular))
  (* Lookup the closure and evaluate it with the passed parameter *)
  | Call (f, arg) -> (
      let arg_val = eval arg environment trust in
      match
        try Env.find f environment
        with Not_found -> raise (UnboundException ("Unbound symbol: " ^ f))
      with
      | Closure (arg_id, body, f_env, sec) -> (
          (* Gateway functions cannot be called by untrusted code *)
          match trust with
          | Low when sec = Enclaved ->
              raise
                (SecurityException
                   "Untrusted code cannot call gateway functions.")
          | _ ->
              let new_env = Env.add arg_id arg_val f_env in
              eval body new_env trust)
      | _ -> failwith "Not a closure.")
  (* Get the new environment obtained by evaluating the enclave *)
  | Enclave (body, rest) ->
      let new_env = eval_enc body environment environment in
      eval rest new_env trust
  | Secret _ -> failwith "Not in an enclave."
  | Gateway _ -> failwith "Not in an enclave."
  (* Include a piece of untrusted code *)
  | Include (id, body, rest) -> (
      match Env.find_opt id environment with
      | Some _ -> raise (DuplicateException ("Symbol already exists: " ^ id))
      | None ->
          let updated_env = Env.add id (UntrustedCode body) environment in
          eval rest updated_env trust)
  (* Evaluate a piece of untrusted code *)
  | Execute (id, rest) ->
      (* The result of the evaluation is discarded *)
      let _ =
        match
          try Env.find id environment
          with Not_found -> raise (UnboundException ("Unbound symbol: " ^ id))
        with
        | UntrustedCode body -> eval body environment Low
        | _ -> failwith "Not untrusted code."
      in
      eval rest environment Normal
  | Print (e, rest) ->
      let v = eval e environment trust in
      let _ = string_of_val v |> print_endline in
      eval rest environment trust
(* | _ -> failwith "Unimplemented." *)

(** Return the environment obtained by evaluating [e].
    We carry the [global_env] with us to resume normal evaluation upon exit from the enclave. *)
and eval_enc (e : expr) (global_env : env) (enc_env : env) : env =
  match e with
  (* A secret is only visible in the enclave environment *)
  | Secret (id, body, rest) ->
      let v = eval body enc_env High in
      let new_env = Env.add id v enc_env in
      eval_enc rest global_env new_env
  (* Marking a function as gateway makes it visible in the global environment *)
  | Gateway (id, rest) -> (
      match
        try Env.find id enc_env
        with Not_found -> raise (UnboundException ("Unbound symbol: " ^ id))
      with
      | Closure (arg_id, body, closure_env, _) ->
          let new_enc_env =
            Env.add id (Closure (arg_id, body, closure_env, Enclaved)) enc_env
          in
          let new_global_env =
            Env.add id
              (Closure (arg_id, body, closure_env, Enclaved))
              global_env
          in
          eval_enc rest new_global_env new_enc_env
      | _ -> failwith "Only functions can be gateways.")
  (* We can declare variables and functions as usual *)
  | Let (id, body, rest) -> (
      let v = eval body enc_env High in
      match v with
      | Closure _ ->
          let new_enc_env = Env.add id v enc_env in
          eval_enc rest global_env new_enc_env
      | _ ->
          let new_global_env = Env.add id v global_env in
          let new_enc_env = Env.add id v enc_env in
          eval_enc rest new_global_env new_enc_env)
  | _ -> global_env

(** Returns the AST obtained from the string [s]. *)
let parse (s : string) : expr option =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** Interprets [s] by lexing it, parsing it,
    evaluating it and converting the result to a string.*)
let interpret (s : string) : string =
  match parse s with
  | Some e -> eval e Env.empty Normal |> string_of_val
  (* Ignore EOF. Is there a better way? *)
  | None -> ""

(** Opens [filename] and interprets its contents.
    Returns the result of the interpretation as a string.*)
let interpret_file (filename : string) : string =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  let ast = Parser.prog Lexer.read lexbuf in
  match ast with
  | Some e -> eval e Env.empty Normal |> string_of_val
  | None -> ""
