
type link = Unbound of string | Linkto of ty [@@deriving show]

and ty =
  | Tunknown
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

and expr =
  | Evar of ty list * string
  | Econstant of constant
  | Etuple of expr list
  | EBinary of binary * expr * expr
  | Eassign of expr * expr
  | Etag of string
  | Eunit
  | Econstruct of string * expr
  | EUnary of unary * expr
  | ESizeof of ty
  | EPostfix of expr * postfix
  | Estruct of (string * expr) list
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

and unary = Plus | Minus | BitNot | LogNot | Ref | Sizeof
[@@deriving show]

and postfix =
  | PCall of expr list
  | PIdx of expr
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

and stmt =
  | SLet of (string * ty) * expr
  | SStmts of stmt list
  | SWhile of expr * stmt
  | SFor of expr * expr * stmt
  | SIfElse of expr * stmt * stmt
  | SReturn of expr option
  | SContinue
  | SBreak
  | SSwitch of expr * (pat * stmt) list
  | SExpr of expr
  | SNone
[@@deriving show]


and def =
  | Deflet of (string * ty) * expr
  | Defimpl of ty list * ty * def list
  | Defstruct of string * ty list * (string * ty) list
  | Defenum of string * ty list * (string * ty) list
  | Deffun of string *  ty list * ty * (string * ty) list * stmt
  | Defmethod of string * ty list * ty * ty list * ty * (string * ty) list
[@@deriving show]

type dl = def list
[@@deriving show]