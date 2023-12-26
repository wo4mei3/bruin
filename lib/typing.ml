
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
type ty =
  | Syntax.Tvar of string
  | Syntax.Tunit
  | Syntax.Tbool
  | Syntax.Tint
  | Syntax.Tfloat
  | Syntax.Tchar
  | Syntax.Tstring
  | Syntax.Tlist of ty
  | Syntax.Tptr of ty
  | Syntax.Ttuple of ty list
  | Syntax.Tconstr of string * ty list
  | Syntax.Tenum of string list * (string * ty) list
  | Syntax.Tstruct of string list * (string * ty) list
  | Syntax.Tfun of string list * ty * ty list
  | Syntax.Ttag
[@@deriving show]
*)

let extract_tvars = function
    | Syntax.Tconstr(name,[]) -> name
    | _ -> failwith "expected type variable"

let valid_ty tvars = function
| Syntax.Tconstr(name,[]) when List.mem name tvars -> ()
| Syntax.Tconstr(_,[]) -> failwith "valid_ty"
| _ -> ()

let valid_fields tvars fields =
  List.iter (fun (_,ty) -> valid_ty tvars ty) fields


let rec make_tyenv' = function
  | Syntax.Defenum(name,tyl,fields)::xs ->
    let tvars = List.map extract_tvars tyl in
    List.iter (fun (_,ty) -> valid_ty tvars ty) fields;
    (name,Syntax.Tenum(name,tvars,fields))::make_tyenv' xs
  | Syntax.Defstruct(name,tyl,fields)::xs ->
    let tvars = List.map extract_tvars tyl in
    List.iter (fun (_,ty) -> valid_ty tvars ty) fields;
    (name,Syntax.Tstruct(name,tvars,fields))::make_tyenv' xs
  | Syntax.Deffun(name,tyl,ret_ty,params,_)::xs ->
    let tvars = List.map extract_tvars tyl in
    valid_ty tvars ret_ty;
    List.iter (fun (_,ty) -> valid_ty tvars ty) params;
    (name,Syntax.Tfun(tvars,ret_ty,List.map snd params))::make_tyenv' xs
  | [] -> []
  | _::xs -> make_tyenv' xs

let rec tconstr_to_ty tyenv' = function
| Syntax.Tlist ty' -> Syntax.Tlist (tconstr_to_ty tyenv' ty')
| Syntax.Tptr ty' -> Syntax.Tptr (tconstr_to_ty tyenv' ty')
| Syntax.Ttuple tyl -> Syntax.Ttuple (List.map (tconstr_to_ty tyenv') tyl)
| Syntax.Tconstr(name,tyl) -> 
  let ty' = instantiate name tyl tyenv' in
  tconstr_to_ty tyenv' ty'
| Syntax.Tenum(name,tvars,fields') ->
  let fields = List.map (fun (name,ty')-> (name,tconstr_to_ty tyenv' ty')) fields' in
  Syntax.Tenum(name,tvars,fields)
| Syntax.Tstruct(name,tvars,fields') ->
  let fields = List.map (fun (name,ty')-> (name,tconstr_to_ty tyenv' ty')) fields' in
  Syntax.Tstruct(name,tvars,fields)
| Syntax.Tfun(tvars,ret_ty',params') ->
  let ret_ty = tconstr_to_ty tyenv' ret_ty' in
  let params = List.map (tconstr_to_ty tyenv') params' in
  Syntax.Tfun(tvars,ret_ty,params)
| ty -> ty


let instantiate name tparams tyenv =
  match ty with
  | Syntax.Tconstr({contents = Unbound lname},tyl) ->


let type_expr env = function
| Syntax.Evar(tyl,name) -> 
  let gen_ty = List.assoc name env in
  let ty = instantiate gen_ty tyl in
  Evar(ty,tyl,name)
| _ -> Eunit Syntax.Tunit

*)