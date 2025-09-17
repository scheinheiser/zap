open Util

type func_env = (string * Ast.located_ty) list
type variable_env = (string * Ast.located_ty) list

type env =
  { func_env : func_env
  ; var_env : variable_env
  }

(* utils *)
let flatten_arrow (arrow : Ast.located_ty) : Ast.located_ty list =
  let rec go acc = function
    | _, Ast.Arrow (l, r) -> go (l :: acc) r
    | t -> List.rev (t :: acc)
  in
  go [] arrow
;;

let rec build_arrow = function
  | [] ->
    raise
      (Failure "Internal error - there should be at least a return type.")
  | [ t ] -> t
  | ((loc', _) as h') :: t -> loc', Ast.Arrow (h', build_arrow t)

(* custom equality function for types that respects type variables *)
let rec ( &= ) (l : Ast.ty) (r : Ast.ty) : bool =
  let open Ast in
  let is_internal = String.starts_with ~prefix:"t_" in
  match l, r with
  | Prim (PGeneric g), _ when is_internal g -> true
  | _, Prim (PGeneric g) when is_internal g -> true
  | Prim l', Prim r' -> l' = r'
  | Tuple l', Tuple r' when List.length l' = List.length r' ->
    List.fold_left2 (fun acc (_, t1) (_, t2) -> acc && t1 &= t2) true l' r'
  | List (_, l'), List (_, r') -> l' &= r'
  | Udt l', Udt r' -> l' = r'
  | Arrow ((_, l1'), (_, r1')), Arrow ((_, l2'), (_, r2')) -> l1' &= l2' && r1' &= r2'
  | _ -> false
;;

let ( &!= ) l r : bool = not (l &= r)

let fresh_env () = {var_env = []; func_env = []}

(* initialise function type environment from the definition list in `Ast.program` *)
let init_func_env (env : env) (defs : Ast.definition list) : env =
  { env with
    func_env =
      List.filter_map
        (function
          | Ast.Dec (i, t) -> Some (i, t)
          | _ -> None)
        defs
  }
;;

let combine_env (l: env) (r: env): env =
  {func_env = l.func_env @ r.func_env; var_env = l.var_env @ r.var_env}

let get_var_type ({ var_env; _ } : env) (var : Ast.ident) : Ast.located_ty option =
  List.assoc_opt var var_env
;;

let get_func_type ({ func_env; _ } : env) (func : Ast.ident) : Ast.located_ty option =
  List.assoc_opt func func_env
;;

let var_exists ({ var_env; _ } : env) (var : Ast.ident) : bool =
  List.mem_assoc var var_env
;;

let func_exists ({ func_env; _ } : env) (func : Ast.ident) : bool =
  List.mem_assoc func func_env
;;

let add_var_type (env : env) (var : Ast.ident) (ty : Ast.located_ty) : env =
  { env with var_env = (var, ty) :: env.var_env }
;;

let add_func_type (env : env) (func : Ast.ident) (ty : Ast.located_ty) : env =
  { env with func_env = (func, ty) :: env.func_env }
;;

let make_err (e : Error.t) : 'a Base.Or_error.t =
  Base.Or_error.error_string @@ Error.format_err e
;;

let fresh_tyvar =
  let i = ref (-1) in
  fun () ->
    incr i;
    "t_" ^ string_of_int !i
;;

(* main stuff *)
let check_list ~(f : env -> 'a -> ('b * env) Base.Or_error.t) (env: env) (l: 'a list) ~(rev: bool): (('b Base.Or_error.t) list) * env =
  let rec go acc e = function
    | [] when rev -> List.rev acc, e
    | [] -> acc, e
    | h :: t ->
      (match f e h with
      | Ok (h', e') -> go (Ok h' :: acc) e' t
      | Error _ as h' -> go (h' :: acc) e t)
  in go [] env l

let get_const_type ((loc, c) : Ast.located_const) : Ast.located_ty =
  let open Ast in
  let t =
    match c with
    | Int _ -> Prim PInt
    | Float _ -> Prim PFloat
    | String _ -> Prim PString
    | Char _ -> Prim PChar
    | Bool _ -> Prim PBool
    | Atom _ -> Prim PAtom
    | Unit -> Prim PUnit
  in
  loc, t
;;

let rec get_pattern_type (env : env) ((loc, pat) : Ast.located_pattern)
  : (Ast.located_ty * env) Base.Or_error.t
  =
  let is_internal = String.starts_with ~prefix:"t_" in
  let open Ast in
  let open Base.Or_error in
  match pat with
  | PConst c -> Ok (get_const_type c, env)
  | PIdent i ->
    let t = loc, Prim (PGeneric (fresh_tyvar ())) in
    let env' = add_var_type env i t in
    Ok (t, env')
  | PList l | PTup l ->
    (match l with
     | [] -> Ok ((loc, Prim (PGeneric (fresh_tyvar ()))), env)
     | items ->
       let ty_list, env' = check_list ~f:get_pattern_type ~rev:true env items in
       combine_errors ty_list
       >>= fun l' ->
       let non_internal =
         List.find_opt
           (function
             | _, Prim (PGeneric g) when is_internal g -> false
             | _ -> true)
           l'
       in
       (match non_internal with
        | None -> Ok ((loc, Prim (PGeneric (fresh_tyvar ()))), env')
        | Some (type_loc, t) ->
          let wts =
            List.filter_map
              (fun (_, t') -> if t' &= t then None else Some (show_ty t'))
              l'
          in
          (match wts with
           | [] -> Ok ((type_loc, t), env')
           | es_types ->
             make_err
               ( Some loc
               , Printf.sprintf
                   "Expected type %s, but got type %s instead."
                   (show_ty t)
                   (String.concat ", " es_types) ))))
  | PCons (l, r) ->
    get_pattern_type env l
    >>= fun ((loc1, l'), e) ->
    get_pattern_type e r
    >>= fun ((loc2, r'), env') ->
    let loc' = Location.combine loc1 loc2 in
    (match r' with
     | List (_, t) ->
       if t &= l'
       then Ok ((loc', List (loc', t)), env')
       else
         make_err
           ( Some loc'
           , Printf.sprintf
               "Expected type %s, but got type %s instead."
               (show_ty t)
               (show_ty l') )
     | t ->
       make_err
         ( Some loc'
         , Printf.sprintf "Expected a list type, but got type %s instead." (show_ty t) ))
  | PWild -> Ok ((loc, Prim (PGeneric (fresh_tyvar ()))), env)
;;

let rec check_expr (env : env) ((loc, e) : Ast.located_expr)
  : Typed_ast.typed_expr Base.Or_error.t
  =
  let open Ast in
  let open Base.Or_error in
  match e with
  | Const c -> Ok (get_const_type c, (loc, Typed_ast.Const c))
  | EList es ->
    let es' = List.map (fun e -> check_expr env e) es in
    combine_errors es'
    >>= fun exprs ->
    (match exprs with
     | [] -> Ok ((loc, Prim (PGeneric (fresh_tyvar ()))), (loc, Typed_ast.EList []))
     | (((ty_loc, ht) as t), _) :: tl ->
       let wts =
         List.filter_map
           (fun ((_, t'), _) -> if t' &= ht then None else Some (show_ty t'))
           tl
       in
       (match wts with
        | [] -> Ok ((ty_loc, List t), (loc, Typed_ast.EList exprs))
        | es_types ->
          make_err
            ( Some loc
            , Printf.sprintf
                "Expected type %s, but got '%s' instead."
                (show_ty ht)
                (String.concat ", " es_types) )))
  | Ident i ->
    (match get_var_type env i with
     | None ->
       (match get_func_type env i with
        | None -> make_err (Some loc, Printf.sprintf "Undefined identifier - %s." i)
        | Some t -> Ok (t, (loc, Typed_ast.Ident i)))
     | Some t -> Ok (t, (loc, Typed_ast.Ident i)))
  | Ap (l, r) ->
    (*TODO: recognise builtins e.g. print *)
    check_expr env l
    >>= fun ((t, _) as l') ->
    check_expr env r
    >>= fun ((t', _) as r') ->
    let _, left_conn = List.hd (flatten_arrow t)
    and ty_loc, right_conn = List.rev (flatten_arrow t') |> List.hd in
    if left_conn &= right_conn
    then Ok ((ty_loc, right_conn), (loc, Typed_ast.Ap (l', r')))
    else
      make_err
        ( Some loc
        , Printf.sprintf
            "Expected type %s, but got %s."
            (show_ty left_conn)
            (show_ty right_conn) )
  | Bop (l, op, r) ->
    check_expr env l
    >>= fun (((_, t), _) as l') ->
    check_expr env r
    >>= fun (((ty_loc, t'), _) as r') ->
    (match op with
     | (Add | Mul | Div | Sub) when t &= t' ->
       if t &!= Prim PInt && t &!= Prim PFloat
       then
         make_err
           (Some loc, Printf.sprintf "Expected type int or float, but got %s." (show_ty t))
       else 
         (match t with
         | Prim PFloat -> Ok ((ty_loc, Prim PFloat), (loc, Typed_ast.Bop (l', op, r')))
         | _ -> Ok ((ty_loc, Prim PInt), (loc, Typed_ast.Bop (l', op, r'))))
     | (And | Or) when t &!= Prim PBool ->
       make_err (Some loc, Printf.sprintf "Expected type bool, but got %s." (show_ty t))
     | (And | Or) when t' &!= Prim PBool ->
       make_err (Some loc, Printf.sprintf "Expected type bool, but got %s." (show_ty t))
     | And | Or -> Ok ((ty_loc, t), (loc, Typed_ast.Bop (l', op, r')))
     | (Less | Greater | LessE | GreaterE | Equal | NotEq) when t &= t' ->
       Ok ((ty_loc, Prim PBool), (loc, Typed_ast.Bop (l', op, r')))
     | Cons ->
       (match t' with
        | List (_, list_type) ->
          if t &= list_type
          then Ok ((ty_loc, list_type), (loc, Typed_ast.Bop (l', op, r')))
          else
            make_err
              ( Some loc
              , Printf.sprintf
                  "Expected type %s, but got %s."
                  (show_ty list_type)
                  (show_ty t) )
        | Prim (PGeneric g) when String.starts_with ~prefix:"t_" g ->
          Ok ((ty_loc, List (ty_loc, t)), (loc, Typed_ast.Bop (l', op, r')))
        | err_type ->
          make_err
            ( Some loc
            , Printf.sprintf
                "Expected an empty list or a list type, but got %s."
                (show_ty err_type) ))
     | User_op i ->
       (match get_func_type env i with
        | None -> make_err (Some loc, Printf.sprintf "Undefined operator - %s." i)
        | Some ((_, esig) as fsig) ->
          (match flatten_arrow fsig with
           | [ (_, lt); (_, rt); ret ] ->
             (match () with
              | _ when lt &!= t ->
                make_err
                  ( Some loc
                  , Printf.sprintf
                      "Expected type %s, but got %s."
                      (show_ty lt)
                      (show_ty t) )
              | _ when rt &!= t' ->
                make_err
                  ( Some loc
                  , Printf.sprintf
                      "Expected type %s, but got %s."
                      (show_ty rt)
                      (show_ty t') )
              | _ -> Ok (ret, (loc, Typed_ast.Bop (l', op, r'))))
           | _ ->
             make_err
               ( Some loc
               , Printf.sprintf
                   "Expected binary operator, but got operator with signature %s."
                   (show_ty esig) )))
     | _ ->
       make_err
         ( Some loc
         , Printf.sprintf "Expected type %s, but got %s." (show_ty t) (show_ty t') ))
;;

let rec check_term (env : env) ((loc, t) : Ast.located_term)
  : (Typed_ast.typed_term * env) Base.Or_error.t
  =
  let open Ast in
  let open Base.Or_error in
  match t with
  | TExpr e ->
    check_expr env e >>| fun (t', e') -> (t', (loc, Typed_ast.TExpr (t', e'))), env
  | TLet (i, _, _) when var_exists env i || func_exists env i ->
    make_err (Some loc, Printf.sprintf "Variable already defined - '%s'." i)
  | TLet (i, typ, v) ->
    check_term env v
    >>= fun ((((type_loc, t'), _) as v'), _) ->
    (match typ with
     | Some (loc', typ') ->
       if typ' &= t'
       then (
         let env' = add_var_type env i (loc', typ') in
         Ok (((loc', typ'), (loc, Typed_ast.TLet (i, v'))), env'))
       else
         make_err
           ( Some loc
           , Printf.sprintf
               "Expected type %s, but got type %s."
               (show_ty typ')
               (show_ty t') )
     | None ->
       let env' = add_var_type env i (type_loc, t') in
       Ok (((type_loc, t'), (loc, Typed_ast.TLet (i, v'))), env'))
  | TGrouping ts ->
    let ts', _ = check_list ~f:check_term ~rev:true env ts in
    combine_errors ts'
    >>= fun ts'' ->
    let typ, _ = List.rev ts'' |> List.hd in
    Ok ((typ, (loc, Typed_ast.TGrouping ts'')), env)
  | TIf (cond, texpr, fexpr) ->
    check_expr env cond
    >>= fun cond' ->
    (match cond' with
     | (_, Prim PBool), _ ->
       check_term env texpr
       >>= fun ((((type_loc, t'), _) as texpr'), _) ->
       (match fexpr with
        | None -> Ok (((type_loc, t'), (loc, Typed_ast.TIf (cond', texpr', None))), env)
        | Some fb ->
          check_term env fb
          >>= fun ((((_, t''), _) as fexpr'), _) ->
          if t' &= t''
          then
            Ok (((type_loc, t'), (loc, Typed_ast.TIf (cond', texpr', Some fexpr'))), env)
          else
            make_err
              ( Some loc
              , Printf.sprintf
                  "Expected type %s, but got type %s."
                  (show_ty t')
                  (show_ty t'') ))
     | (_, t), _ ->
       make_err (Some loc, Printf.sprintf "Expected type bool, but got %s." (show_ty t)))
  | TTup contents ->
    List.map (fun e -> check_expr env e) contents
    |> combine_errors
    >>= fun values ->
    let tup_types = List.map (fun (t, _) -> t) values in
    let tup = loc, Ast.Tuple tup_types in
    Ok ((tup, (loc, Typed_ast.TTup values)), env)
  | TLam (args, body) ->
    let x, env' = check_list ~f:get_pattern_type ~rev:false env args in
    combine_errors x
    >>= fun tys ->
    check_term env' body
    >>= fun (((ret, _) as body'), _) ->
    let tys' = ret :: tys |> List.rev |> build_arrow in
    Ok ((tys', (loc, Typed_ast.TLam (args, body'))), env)
;;

let rec check_def (env: env) ((loc, d) : Ast.located_definition): (Typed_ast.located_definition list * env) Base.Or_error.t =
  let open Ast in
  let open Base.Or_error in
  match d with
  | Dec _ -> raise (Failure "Internal error - no dec in check_dec.")
  | Def (i, args, when_block, body, with_block) ->
    let func_type = get_func_type env i in
    let wb_res =
      (match with_block with
      | None -> Ok ([], env)
      | Some ds ->
        let ds' = List.filter_map (function _, Dec _ -> None | def -> Some def) ds in
        let env' = init_func_env (fresh_env ()) (List.map (fun (_, d') -> d') ds) |> combine_env env in
        let ds'', env'' = check_list ~f:check_def ~rev:true env' ds' in
        combine_errors ds'' >>| fun defs -> (List.flatten defs, env''))
    in
    wb_res
    >>= fun (with_block', e) ->
    let args', env' = check_list ~f:get_pattern_type ~rev:false e args in
    combine_errors args' >>= fun typed_args ->
    let when_block' = 
      match when_block with
      | None -> Ok None
      | Some wb ->
        check_term env' wb >>= fun (((type_loc, wb_type), _) as wb'', _) ->
        if wb_type &= Prim PBool
        then Ok (Some wb'')
        else make_err (Some type_loc, Printf.sprintf "Expected type bool, but got type %s." (show_ty wb_type))
    in
    when_block' >>= fun wb ->
    let body', _ = check_list ~f:check_term ~rev:true env' body in
    combine_errors body' >>= fun typed_body ->
    let ((last_loc, last), _) = List.rev typed_body |> List.hd in
    let ret_loc, ret = ((last_loc, last) :: typed_args) |> List.rev |> build_arrow in
    (match func_type with
    (*TODO: emit a warning here *)
    | None ->
      let env'' = add_func_type env i (ret_loc, ret) in
      let d = (loc, (i, (ret_loc, ret), args, wb, typed_body)) in
      Ok ((d :: with_block'), env'')
    | Some ((_, t) as dec_ty) when t &= ret ->
      let d = (loc, (i, dec_ty, args, wb, typed_body)) in
      Ok ((d :: with_block'), env')
    | Some (_, t) ->
      make_err (Some loc, Printf.sprintf "Expected type %s, but got type %s." (show_ty t) (show_ty ret)))
