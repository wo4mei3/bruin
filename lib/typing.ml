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
type stmt = expr Syntax.stmt [@@deriving show]

let extract_tvars = function
  | Syntax.Tconstr (name, []) -> name
  | _ -> failwith "expected type variable"

let rec valid_ty tvars = function
  | Syntax.Tconstr (name, _) when List.mem name tvars -> ()
  | Syntax.Tconstr (_, []) -> failwith "valid_ty"
  | Syntax.Tconstr (_, l) -> List.iter (valid_ty tvars) l
  | _ -> ()

let rec make_tyenv' = function
  | Syntax.Defenum (name, tyl, fields) :: xs ->
      let tvars = List.map extract_tvars tyl in
      Syntax.Normal (Syntax.Tenum (name, tvars, fields)) :: make_tyenv' xs
  | Syntax.Defstruct (name, tyl, fields) :: xs ->
      let tvars = List.map extract_tvars tyl in
      Syntax.Normal (Syntax.Tstruct (name, tvars, fields)) :: make_tyenv' xs
  | Syntax.Deffun (name, tyl, ret_ty, params, _) :: xs ->
      let tvars = List.map extract_tvars tyl in
      Syntax.Var (name, Syntax.Tfun (tvars, ret_ty, List.map snd params))
      :: make_tyenv' xs
  | [] -> []
  | _ :: xs -> make_tyenv' xs

let make_tyenv dl =
  let tyenv = make_tyenv' dl in
  let rec aux = function
    | Syntax.Normal (Syntax.Tenum (_, tvars, fields)) :: xl ->
        List.iter (fun (_, ty) -> valid_ty tvars ty) fields;
        aux xl
    | Syntax.Normal (Syntax.Tstruct (_, tvars, fields)) :: xl ->
        List.iter (fun (_, ty) -> valid_ty tvars ty) fields;
        aux xl
    | Syntax.Var (_, Syntax.Tfun (tvars, ret_ty, tyl)) :: xl ->
        valid_ty tvars ret_ty;
        List.iter (valid_ty tvars) tyl;
        aux xl
    | _ :: xl -> aux xl
    | [] -> []
  in
  aux tyenv

let rec tconstr_to_ty tyenv' = function
  | Syntax.Tlist ty' -> Syntax.Tlist (tconstr_to_ty tyenv' ty')
  | Syntax.Tptr ty' -> Syntax.Tptr (tconstr_to_ty tyenv' ty')
  | Syntax.Ttuple tyl -> Syntax.Ttuple (List.map (tconstr_to_ty tyenv') tyl)
  | Syntax.Tconstr (_, tyl) as ty -> instantiate (Syntax.Normal ty) tyl tyenv'
  | Syntax.Tenum (name, tvars, fields') ->
      let fields =
        List.map (fun (name, ty') -> (name, tconstr_to_ty tyenv' ty')) fields'
      in
      Syntax.Tenum (name, tvars, fields)
  | Syntax.Tstruct (name, tvars, fields') ->
      let fields =
        List.map (fun (name, ty') -> (name, tconstr_to_ty tyenv' ty')) fields'
      in
      Syntax.Tstruct (name, tvars, fields)
  | Syntax.Tfun (tvars, ret_ty', params') ->
      let ret_ty = tconstr_to_ty tyenv' ret_ty' in
      let params = List.map (tconstr_to_ty tyenv') params' in
      Syntax.Tfun (tvars, ret_ty, params)
  | ty -> ty

and unify ty obj_ty =
  let rec aux ty obj_ty =
    match (ty, obj_ty) with
    | Syntax.Tvar s, ty -> [ (s, ty) ]
    | Syntax.Tlist ty, Syntax.Tlist obj_ty -> aux ty obj_ty
    | Syntax.Tptr ty, Syntax.Tptr obj_ty -> aux ty obj_ty
    | Syntax.Ttuple tyl1, Syntax.Ttuple tyl2 ->
        List.flatten (List.map2 aux tyl1 tyl2)
    | Syntax.Tconstr (s1, tyl1), Syntax.Tconstr (s2, tyl2) when s1 = s2 ->
        List.flatten (List.map2 aux tyl1 tyl2)
    | Syntax.Tenum (s1, _, f1), Syntax.Tenum (s2, _, f2) when s1 = s2 ->
        List.flatten (List.map2 (fun (_, t1) (_, t2) -> aux t1 t2) f1 f2)
    | Syntax.Tstruct (s1, _, f1), Syntax.Tstruct (s2, _, f2) when s1 = s2 ->
        List.flatten (List.map2 (fun (_, t1) (_, t2) -> aux t1 t2) f1 f2)
    | Syntax.Tfun (_, r1, p1), Syntax.Tfun (_, r2, p2) ->
        aux r1 r2 @ List.flatten (List.map2 aux p1 p2)
    | t1, t2 when t1 = t2 -> []
    | _ -> failwith "unify failed"
  in
  try Some (aux ty obj_ty) with _ -> None

and get_typarams = function
  | Syntax.Tenum (_, l, _) | Syntax.Tstruct (_, l, _) | Syntax.Tfun (l, _, _) ->
      l
  | _ -> []

and do_subst subst = function
  | Syntax.Tvar s when List.mem_assoc s subst -> List.assoc s subst
  | Syntax.Tlist ty -> Syntax.Tlist (do_subst subst ty)
  | Syntax.Tptr ty -> Syntax.Tptr (do_subst subst ty)
  | Syntax.Ttuple tyl -> Syntax.Ttuple (List.map (do_subst subst) tyl)
  | Syntax.Tconstr (s, tyl) -> Syntax.Tconstr (s, List.map (do_subst subst) tyl)
  | Syntax.Tenum (s, l, f) ->
      Syntax.Tenum (s, l, List.map (fun (s, t) -> (s, do_subst subst t)) f)
  | Syntax.Tstruct (s, l, f) ->
      Syntax.Tstruct (s, l, List.map (fun (s, t) -> (s, do_subst subst t)) f)
  | Syntax.Tfun (l, r, p) ->
      Syntax.Tfun (l, do_subst subst r, List.map (do_subst subst) p)
  | ty -> ty

and assoc p = function
  | [] -> failwith "assoc"
  | (Syntax.Normal ty as path) :: _ when Option.is_some (p path) ->
      do_subst (Option.get (p path)) ty
  | (Syntax.Method (_, ty) as path) :: _ when Option.is_some (p path) ->
      do_subst (Option.get (p path)) ty
  | _ :: l -> assoc p l

and instantiate path tyl tyenv =
  let pred path' =
    match (path, path') with
    | Syntax.Normal ty, Syntax.Normal obj_ty
      when Option.is_some (unify (tconstr_to_ty tyenv ty) obj_ty) ->
        unify (tconstr_to_ty tyenv ty) obj_ty
    | Syntax.Method (s1, ty), Syntax.Method (s2, obj_ty)
      when s1 = s2 && Option.is_some (unify (tconstr_to_ty tyenv ty) obj_ty) ->
        unify (tconstr_to_ty tyenv ty) obj_ty
    | _ -> None
  in
  let ty = assoc pred tyenv in
  let subst = List.combine (get_typarams ty) tyl in
  do_subst subst ty

let rec assoc_env name = function
  | Syntax.Var (n, ty) :: _ when n = name -> ty
  | _ :: l -> assoc_env name l
  | [] -> failwith name

let type_expr env = function
  | Syntax.Evar (tyl, name) ->
      let gen_ty = assoc_env name env in
      let ty = instantiate (Normal gen_ty) tyl env in
      (ty, Evar (ty, tyl, name), Syntax.Var (name, ty) :: env)
  | _ -> (Syntax.Tunit, Eunit Syntax.Tunit, env)

(*
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
*)

let rec type_stmt env = function
  | Syntax.SLet ((name, t), e) ->
      let ty, e, env = type_expr env e in
      if t = ty then (Syntax.SLet ((name, ty), e), env)
      else failwith "type of lhs and rhs of let unmatched"
  | Syntax.SStmts l ->
      let env, l =
        List.fold_left
          (fun (env, l) s ->
            let s, env = type_stmt env s in
            (env, s :: l))
          (env, []) l
      in
      (Syntax.SStmts l, env)
  | _ -> failwith "not_impl"

let typing dl =
  let env = make_tyenv dl in
  match List.hd dl with
  | Syntax.Deffun (_, _, _, _, stmt) ->
      let s, _ = type_stmt env stmt in
      s
  | _ -> SNone
