type ty =
  | Tunit
  | Tbool
  | Tint
  | Tfloat
  | Tchar
  | Tstring
  | Tlist of ty
  | Tptr of ty
  | Ttuple of ty list
  | Tvar of string * ty list
  | Tenum of string * ty list * (string * ty) list
  | Tstruct of string * ty list * (string * ty) list
  | Tfun of ty list * ty * ty list
  | Ttag
[@@deriving show]

type expr =
  | Evar of ty list * string
  | Econstant of constant
  | Etuple of expr list
  | EBinary of binary * expr * expr
  | Eassign of expr * expr
  | Etag of ty * string
  | Eunit
  | Econstruct of ty * string * expr
  | EUnary of unary * expr
  | ESizeof of ty
  | EPostfix of expr * expr postfix
  | Estruct of ty * (string * expr) list
[@@deriving show]

and constant =
  | Cint of int
  | Cbool of bool
  | Cfloat of float
  | Cstring of string
  | Cchar of char
[@@deriving show]

and binary =
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

and unary = Plus | Minus | BitNot | LogNot | Ref | Sizeof [@@deriving show]

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
  | Ptag of ty * string
  | Pconstruct of ty * string * pat
  | Pstruct of ty * (string * pat) list
[@@deriving show]

and 'expr stmt =
  | SLet of (string * ty) * 'expr
  | SStmts of 'expr stmt list
  | SWhile of 'expr * 'expr stmt
  | SFor of 'expr * 'expr * 'expr stmt
  | SIfElse of 'expr * 'expr stmt * 'expr stmt
  | SReturn of 'expr option
  | SContinue
  | SBreak
  | SSwitch of 'expr * (pat * 'expr stmt) list
  | SExpr of 'expr
  | SNone
[@@deriving show]

and 'expr def =
  | Deflet of (string * ty) * 'expr
  | Defimpl of ty list * ty * 'expr def list
  | Defstruct of string * ty list * (string * ty) list
  | Defenum of string * ty list * (string * ty) list
  | Deffun of string * ty list * ty * (string * ty) list * 'expr stmt
  | Defmethod of
      string * ty option * ty list * ty * (string * ty) list * 'expr stmt
[@@deriving show]

type dl = expr def list [@@deriving show]

type path = Var of string * ty | Normal of ty | Member of string * ty
[@@deriving show]

type pl = path list [@@deriving show]
