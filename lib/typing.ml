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
  | Syntax.Tvar (name, []) -> name
  | _ -> failwith "expected type variable"

let rec valid_ty tvars = function
  | Syntax.Tvar (name, _) when List.mem name tvars -> ()
  | Syntax.Tvar (_, []) -> failwith "valid_ty"
  | Syntax.Tvar (_, l) -> List.iter (valid_ty tvars) l
  | _ -> ()

let rec make_tyenv' = function
  | Syntax.Defenum (name, tyl, fields) :: xs ->
      let tvars = List.map extract_tvars tyl in
      List.iter (fun (_, ty) -> valid_ty tvars ty) fields;
      Syntax.Normal (Syntax.Tenum (name, tyl, fields)) :: make_tyenv' xs
  | Syntax.Defstruct (name, tyl, fields) :: xs ->
      let tvars = List.map extract_tvars tyl in
      List.iter (fun (_, ty) -> valid_ty tvars ty) fields;
      Syntax.Normal (Syntax.Tstruct (name, tyl, fields)) :: make_tyenv' xs
  | Syntax.Deffun (name, tyl, ret_ty, params, _) :: xs ->
      let tvars = List.map extract_tvars tyl in
      valid_ty tvars ret_ty;
      List.iter (valid_ty tvars) tyl;
      Syntax.Var (name, Syntax.Tfun (tyl, ret_ty, List.map snd params))
      :: make_tyenv' xs
  | [] -> []
  | _ :: xs -> make_tyenv' xs

let rec assoc_env name = function
  | Syntax.Var (n, ty) :: _ when n = name -> ty
  | Syntax.Normal (Syntax.Tvar (n, _) as ty) :: _ when n = name -> ty
  | Syntax.Normal (Syntax.Tenum (n, _, _) as ty) :: _ when n = name -> ty
  | Syntax.Normal (Syntax.Tstruct (n, _, _) as ty) :: _ when n = name -> ty
  | _ :: l -> assoc_env name l
  | [] -> failwith name

let rec tvar_to_ty tyenv' = function
  | Syntax.Tlist ty' -> Syntax.Tlist (tvar_to_ty tyenv' ty')
  | Syntax.Tptr ty' -> Syntax.Tptr (tvar_to_ty tyenv' ty')
  | Syntax.Ttuple tyl -> Syntax.Ttuple (List.map (tvar_to_ty tyenv') tyl)
  | Syntax.Tvar (name, tyl) ->
      let ty = assoc_env name tyenv' in
      let subst = List.combine (get_typarams ty) tyl in
      do_subst subst ty
  | Syntax.Tenum (name, tvars, fields') ->
      let add_env = List.map (fun ty -> Syntax.Normal ty) tvars in
      let env = add_env @ tyenv' in
      let fields =
        List.map (fun (name, ty') -> (name, tvar_to_ty env ty')) fields'
      in
      Syntax.Tenum (name, tvars, fields)
  | Syntax.Tstruct (name, tvars, fields') ->
      let add_env = List.map (fun ty -> Syntax.Normal ty) tvars in
      let env = add_env @ tyenv' in
      let fields =
        List.map (fun (name, ty') -> (name, tvar_to_ty env ty')) fields'
      in
      Syntax.Tstruct (name, tvars, fields)
  | Syntax.Tfun (tvars, ret_ty', params') ->
      let add_env = List.map (fun ty -> Syntax.Normal ty) tvars in
      let env = add_env @ tyenv' in
      let ret_ty = tvar_to_ty env ret_ty' in
      let params = List.map (tvar_to_ty env) params' in
      Syntax.Tfun (tvars, ret_ty, params)
  | ty -> ty

and make_tyenv tyenv' = function
  | Syntax.Var (n, ty) :: xs ->
      Syntax.Var (n, tvar_to_ty tyenv' ty) :: make_tyenv tyenv' xs
  | Syntax.Normal ty :: xs ->
      Syntax.Normal (tvar_to_ty tyenv' ty) :: make_tyenv tyenv' xs
  | Syntax.Member (n, ty) :: xs ->
      Syntax.Member (n, tvar_to_ty tyenv' ty) :: make_tyenv tyenv' xs
  | [] -> []

and make_env dl =
  let tyenv' = make_tyenv' dl in
  make_tyenv tyenv' tyenv'

and unify ty obj_ty =
  let rec aux ty obj_ty =
    match (ty, obj_ty) with
    | Syntax.Tvar (s, []), ty -> [ (s, ty) ]
    | Syntax.Tlist ty, Syntax.Tlist obj_ty -> aux ty obj_ty
    | Syntax.Tptr ty, Syntax.Tptr obj_ty -> aux ty obj_ty
    | Syntax.Ttuple tyl1, Syntax.Ttuple tyl2 ->
        List.flatten (List.map2 aux tyl1 tyl2)
    | Syntax.Tvar (s1, tyl1), Syntax.Tvar (s2, tyl2) when s1 = s2 ->
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

and get_typarams ty =
  let aux = function
    | Syntax.Tvar (s, []) -> s
    | _ -> failwith "get_typarams"
  in
  match ty with
  | Syntax.Tenum (_, l, _) | Syntax.Tstruct (_, l, _) | Syntax.Tfun (l, _, _) ->
      List.map aux l
  | _ -> []

and do_subst subst = function
  | Syntax.Tvar (s, []) when List.mem_assoc s subst -> List.assoc s subst
  | Syntax.Tlist ty -> Syntax.Tlist (do_subst subst ty)
  | Syntax.Tptr ty -> Syntax.Tptr (do_subst subst ty)
  | Syntax.Ttuple tyl -> Syntax.Ttuple (List.map (do_subst subst) tyl)
  | Syntax.Tvar (s, tyl) -> Syntax.Tvar (s, List.map (do_subst subst) tyl)
  | Syntax.Tenum (s, l, f) ->
      Syntax.Tenum (s, l, List.map (fun (s, t) -> (s, do_subst subst t)) f)
  | Syntax.Tstruct (s, l, f) ->
      Syntax.Tstruct (s, l, List.map (fun (s, t) -> (s, do_subst subst t)) f)
  | Syntax.Tfun (_, r, p) ->
      Syntax.Tfun ([], do_subst subst r, List.map (do_subst subst) p)
  | ty -> ty

and assoc p = function
  | [] -> failwith "assoc"
  | (Syntax.Var (_, ty) as path) :: _ when Option.is_some (p path) ->
      (Option.get (p path), ty)
  | (Syntax.Normal ty as path) :: _ when Option.is_some (p path) ->
      (Option.get (p path), ty)
  | (Syntax.Member (_, ty) as path) :: _ when Option.is_some (p path) ->
      (Option.get (p path), ty)
  | _ :: l -> assoc p l

and instantiate path tyl tyenv =
  let pred path' =
    match (path, path') with
    | Syntax.Var (s1, ty), Syntax.Var (s2, obj_ty)
      when s1 = s2 && Option.is_some (unify ty obj_ty) ->
        unify ty obj_ty
    | Syntax.Normal ty, Syntax.Normal obj_ty
      when Option.is_some (unify ty obj_ty) ->
        unify ty obj_ty
    | Syntax.Member (s1, ty), Syntax.Member (s2, obj_ty)
      when s1 = s2 && Option.is_some (unify ty obj_ty) ->
        unify ty obj_ty
    | _ -> None
  in
  let subst, ty = assoc pred tyenv in
  let subst = List.combine (get_typarams ty) tyl @ subst in
  do_subst subst ty

let type_expr env = function
  | Syntax.Evar (tyl, name) ->
      let gen_ty = assoc_env name env in
      let ty = instantiate (Var (name, gen_ty)) tyl env in
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
      print_endline
        (Syntax.show_ty (tvar_to_ty env t) ^ " = " ^ Syntax.show_ty ty);
      if tvar_to_ty env t = ty then (Syntax.SLet ((name, ty), e), env)
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
  let env = make_env dl in
  match List.hd dl with
  | Syntax.Deffun (_, tyl, _, _, stmt) ->
      let env1 = List.map (fun ty -> Syntax.Normal ty) tyl in
      let env = make_tyenv (env1 @ env) env1 @ env in
      let s, _ = type_stmt env stmt in
      s
  | _ -> SNone
