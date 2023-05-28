open Ast

(* Security level associated with a closure.
   Used to prevent gateway functions from being called by untrusted code. *)
type sec = Regular | Enclaved

module Env = Map.Make (String)

(* The environment, a map from strings to values *)
type env = value Env.t

(* Values *)
and value =
  | IntV of int
  | BoolV of bool
  | StringV of string
  | Closure of ide * expr * env * sec
  | UntrustedCode of expr
