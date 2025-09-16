open Util

type func_env = (string * Ast.located_ty) list
type variable_env = (string * Ast.located_ty) list

type env =
  { mutable func_env : func_env
  ; mutable var_env : variable_env
  ; mutable tyvar_idx : int
  }

(* utils *)
let flatten_arrow (arrow : Ast.located_ty) : Ast.located_ty list =
  let rec go acc = function
    | _, Ast.Arrow (l, r) -> go (l :: acc) r
    | t -> List.rev (t :: acc)
  in
  go [] arrow
;;

(* initialise function type environment from the definition list in `Ast.program` *)
let init_func_env (env : env) (defs : Ast.definition list) : unit =
  env.func_env
  <- List.filter_map
       (function
         | Ast.Dec (i, t) -> Some (i, t)
         | _ -> None)
       defs
;;

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

let add_var_type
      ({ var_env = venv; _ } as env : env)
      (var : Ast.ident)
      (ty : Ast.located_ty)
  : unit
  =
  env.var_env <- (var, ty) :: venv
;;

let add_func_type
      ({ func_env = fenv; _ } as env : env)
      (func : Ast.ident)
      (ty : Ast.located_ty)
  : unit
  =
  env.func_env <- (func, ty) :: fenv
;;

let make_err (e : Error.t) : 'a Base.Or_error.t =
  Base.Or_error.error_string @@ Error.format_err e

(* main stuff *)
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
     | [] ->
       let tyvar = "internal_" ^ string_of_int env.tyvar_idx in
       env.tyvar_idx <- env.tyvar_idx + 1;
       Ok ((loc, Prim (PGeneric tyvar)), (loc, Typed_ast.EList []))
     | (((ty_loc, ht) as t), _) :: tl ->
       (match (List.filter_map (fun ((_, t'), _) -> if t' = ht then None else Some (show_ty t')) tl) with
       | [] -> Ok ((ty_loc, List t), (loc, Typed_ast.EList exprs))
       | es_types ->
         make_err (Some loc, Printf.sprintf "Expected type %s, but got '%s' instead." (show_ty ht) (String.concat ", " es_types))))
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
    if left_conn = right_conn
    then Ok ((ty_loc, right_conn), (loc, Typed_ast.Ap (l', r')))
    else make_err (Some loc, Printf.sprintf "Expected type %s, but got %s." (show_ty left_conn) (show_ty right_conn))
  | Bop (l, op, r) ->
    check_expr env l
    >>= fun (((_, t), _) as l') ->
    check_expr env r
    >>= fun (((ty_loc, t'), _) as r') ->
    (match op with
     | (Add | Mul | Div | Sub) when t = t' ->
       if t <> Prim PInt && t <> Prim PFloat
       then make_err (Some loc, Printf.sprintf "Expected type int or float, but got %s." (show_ty t))
       else Ok ((ty_loc, t), (loc, Typed_ast.Bop (l', op, r')))
     | (And | Or) when t <> Prim PBool -> make_err (Some loc, Printf.sprintf "Expected type bool, but got %s." (show_ty t))
     | (And | Or) when t' <> Prim PBool -> make_err (Some loc, Printf.sprintf "Expected type bool, but got %s." (show_ty t))
     | And | Or -> Ok ((ty_loc, t), (loc, Typed_ast.Bop (l', op, r')))
     | (Less | Greater | LessE | GreaterE | Equal | NotEq) when t = t' ->
       Ok ((ty_loc, Prim PBool), (loc, Typed_ast.Bop (l', op, r')))
     | Cons ->
       (match t' with
        | List (_, list_type) ->
          if t = list_type
          then Ok ((ty_loc, list_type), (loc, Typed_ast.Bop (l', op, r')))
          else make_err (Some loc, Printf.sprintf "Expected type %s, but got %s." (show_ty list_type) (show_ty t))
        | Prim (PGeneric g) when String.starts_with ~prefix:"internal_" g ->
          Ok ((ty_loc, List (ty_loc, t)), (loc, Typed_ast.Bop (l', op, r')))
        | err_type -> make_err ( Some loc, Printf.sprintf "Expected an empty list or a list type, but got %s." (show_ty err_type)))
     | User_op i ->
       (match (get_func_type env i) with
        | None -> make_err (Some loc, Printf.sprintf "Undefined operator - %s." i)
        | Some ((_, esig) as fsig) ->
          (match flatten_arrow fsig with
          | [ (_, lt); (_, rt); ret ] ->
            (match () with
             | _ when lt <> t -> make_err (Some loc, Printf.sprintf "Expected type %s, but got %s." (show_ty lt) (show_ty t))
             | _ when rt <> t' -> make_err (Some loc, Printf.sprintf "Expected type %s, but got %s." (show_ty rt) (show_ty t'))
             | _ -> Ok (ret, (loc, Typed_ast.Bop (l', op, r'))))
          | _ -> make_err (Some loc, Printf.sprintf "Expected binary operator, but got operator with signature %s." (show_ty esig))))
     | _ -> make_err (Some loc, Printf.sprintf "Expected type %s, but got %s." (show_ty t) (show_ty t')))
;;

(*TODO: propagate internal type variable from empty list/etc*)
let rec check_term (env : env) ((loc, t) : Ast.located_term)
  : Typed_ast.typed_term Base.Or_error.t
  =
  let open Ast in
  let open Base.Or_error in
  match t with
  | TExpr e -> check_expr env e >>| fun (t', e') -> t', (loc, Typed_ast.TExpr (t', e'))
  | TLet (i, _, _) when var_exists env i || func_exists env i -> make_err (Some loc, Printf.sprintf "Variable already defined - '%s'." i)
  | TLet (i, typ, v) ->
    let env' = env in
    check_term env' v
    >>= fun (((type_loc, t'), _) as v') ->
    (match typ with
     | Some (loc', typ') ->
       if typ' <> t'
       then make_err (Some loc, Printf.sprintf "Expected type %s, but got type %s." (show_ty typ') (show_ty t'))
       else (
         add_var_type env i (loc', typ');
         Ok ((loc', typ'), (loc, Typed_ast.TLet (i, v'))))
     | None ->
       add_var_type env i (type_loc, t');
       Ok ((type_loc, t'), (loc, Typed_ast.TLet (i, v'))))
  | TGrouping ts ->
    let env' = env in
    let ts' = List.map (fun t -> check_term env' t) ts in
    combine_errors ts'
    >>= fun ts'' ->
    let typ, _ = List.rev ts'' |> List.hd in
    Ok (typ, (loc, Typed_ast.TGrouping ts''))
  | TIf (cond, texpr, fexpr) ->
    check_expr env cond
    >>= fun cond' ->
    (match cond' with
     | (_, Prim PBool), _ ->
       let env' = env in
       check_term env' texpr
       >>= fun (((type_loc, t'), _) as texpr') ->
       (match fexpr with
        | None -> Ok ((type_loc, t'), (loc, Typed_ast.TIf (cond', texpr', None)))
        | Some fb ->
          let env'' = env in
          check_term env'' fb
          >>= fun (((_, t''), _) as fexpr') ->
          if t' <> t''
          then make_err (Some loc, Printf.sprintf "Expected type %s, but got type %s." (show_ty t') (show_ty t''))
          else Ok ((type_loc, t'), (loc, Typed_ast.TIf (cond', texpr', Some fexpr'))))
     | (_, t), _ ->
       let estr =
         Error.format_err
           (Some loc, Printf.sprintf "Expected type bool, but got %s." (show_ty t))
       in
       Error (Base.Error.of_string estr))
  | TTup contents ->
    List.map (fun e -> check_expr env e) contents
    |> combine_errors
    >>= fun values ->
    let tup_types = List.map (fun (t, _) -> t) values in
    let tup = loc, Ast.Tuple tup_types in
    Ok (tup, (loc, Typed_ast.TTup values))
  | _ -> raise (Failure "TODO: all cases in term type checking.")
;;
