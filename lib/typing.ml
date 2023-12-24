open Ast

type expr =
  | Evar of ty * ty list * string
  | Econstant of ty * constant
  | Etuple of ty * expr list
  | EBinary of ty * binary * expr * expr
  | Eassign of ty * expr * expr
  | Etag of ty * string
  | Eunit of ty
  | Econstruct of ty * string * expr
  | EUnary of ty * unary * expr
  | ESizeof of ty
  | EPostfix of ty * expr * expr postfix
  | Estruct of ty * (string * expr) list
[@@deriving show]

type dl = expr def list [@@deriving show]
