
type expr =
  | Evar of Syntax.ty * Syntax.ty list * string
  | Econstant of Syntax.ty * Syntax.constant
  | Etuple of Syntax.ty * expr list
  | EBinary of Syntax.ty * Syntax.binary * expr * expr
  | Eassign of Syntax.ty * expr * expr
  | Etag of Syntax.ty * string
  | Eunit of Syntax.ty
  | Econstruct of Syntax.ty * string * expr
  | EUnary of Syntax.ty * Syntax.unary * expr
  | ESizeof of Syntax.ty
  | EPostfix of Syntax.ty * expr * expr Syntax.postfix
  | Estruct of Syntax.ty * (string * expr) list
[@@deriving show]

type dl = expr Syntax.def list [@@deriving show]

(*

let instantiate ty tparams =
  match ty with
  | Tconstr({contents = Unbound lname},tyl) ->

let type_expr env = function
| Syntax.Evar(tyl,name) -> 
  let gen_ty = List.assoc name env in
  let ty = instantiate gen_ty tyl in
  Evar(ty,tyl,name)
| _ -> Eunit Syntax.Tunit

*)