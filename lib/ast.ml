(* Identifiers *)
type ide = string

(* Binary operators *)
type op = Add | Mult | Sub | Div | And | Or | Eq | Gt | Gte | Lt | Lte

(* Expressions *)
type expr =
  | Int of int
  | Bool of bool
  | String of string
  | Binop of op * expr * expr
  | Not of expr
  | IfThenElse of expr * expr * expr
  | Let of ide * expr * expr
  | Var of ide
  | Fun of ide * expr
  | Call of ide * expr
  | Enclave of expr * expr
  | Secret of ide * expr * expr
  | Gateway of ide * expr
  | Include of ide * expr * expr
  | Execute of ide * expr
  | Print of expr * expr
