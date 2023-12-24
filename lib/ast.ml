type link = Unbound of string | Linkto of ty [@@deriving show]

and ty =
  | Tvar of link ref
  | Tunit
  | Tbool
  | Tint
  | Tfloat
  | Tchar
  | Tstring
  | Tlist of ty
  | Tptr of ty
  | Tformat of ty * ty
  | Ttuple of ty list
  | Tconstr of string * ty list
  | Tenum of string * ty list * (string * ty) list
  | Tstruct of string * ty list * (string * ty) list
  | Tfun of ty list *ty *  (string * ty) list
  | Ttag
[@@deriving show]

type constant =
  | Cint of int
  | Cbool of bool
  | Cfloat of float
  | Cstring of string
  | Cchar of char
[@@deriving show]

type binary =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | LShift
  | RShift
  | BitAnd
  | BitXor
  | BitOr
  | LogAnd
  | LogOr
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Comma
[@@deriving show]

and unary = Plus | Minus | BitNot | LogNot | Ref | Sizeof
[@@deriving show]

and 'expr postfix =
  | PCall of 'expr list
  | PIdx of 'expr
  | PDot of ty list * string
  | PInc
  | PDec
  | PDeref
[@@deriving show]

and pat =
  | Pwild
  | Pvar of string
  | Palias of pat * string
  | Pconstant of constant
  | Ptuple of pat list
  | Pnil
  | Pcons of pat * pat
  | Pref of pat
  | Punit
  | Ptag of string
  | Pconstruct of string * pat
  | Precord of (string * pat) list
[@@deriving show]

and 'expr stmt =
  | SLet of (string * ty) * 'expr
  | SStmts of 'expr stmt list
  | SWhile of 'expr * 'expr stmt
  | SFor of 'expr *'expr * 'expr stmt
  | SIfElse of 'expr * 'expr stmt * 'expr stmt
  | SReturn of 'expr option
  | SContinue
  | SBreak
  | SSwitch of 'expr * (pat * 'expr stmt) list
  | SExpr of 'expr
  | SNone
[@@deriving show]


and 'expr def =
  | Deflet of (string * ty) *'expr
  | Defimpl of ty list * ty * 'expr def list
  | Defstruct of string * ty list * (string * ty) list
  | Defenum of string * ty list * (string * ty) list
  | Deffun of string *  ty list * ty * (string * ty) list * 'expr stmt
  | Defmethod of string * ty list * ty option * ty list * ty * (string * ty) list * 'expr stmt
[@@deriving show]

module Expr = struct
type t =
  | Evar of ty list * string
  | Econstant of constant
  | Etuple of t list
  | EBinary of binary * t * t
  | Eassign of t * t
  | Etag of string
  | Eunit
  | Econstruct of string * t
  | EUnary of unary * t
  | ESizeof of ty
  | EPostfix of t * t postfix
  | Estruct of (string * t) list
[@@deriving show]

type  dl = t def list
[@@deriving show]

end

module Typed_Expr = struct
type t =
  | Evar of ty* ty list * string
  | Econstant of ty * constant
  | Etuple of ty * t list
  | EBinary of ty * binary * t * t
  | Eassign of  ty * t * t
  | Etag of ty * string
  | Eunit of ty
  | Econstruct of ty * string * t
  | EUnary of ty * unary * t
  | ESizeof of ty
  | EPostfix of ty * t * t postfix
  | Estruct of ty * (string * t) list
[@@deriving show]


type  dl = t def list
[@@deriving show]

end