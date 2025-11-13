(* made using https://www.andrevdm.com/posts/2025-05-29-simple-hm-in-practice.pdf as a reference *)
open Util

(* type map *)
module TM = Map.Make (String)

type 'a base_type_map = 'a TM.t

type env =
  { func_env    : Ast.quantified_ty base_type_map
  ; var_env     : Ast.quantified_ty base_type_map
  ; alias_env   : Ast.quantified_ty base_type_map
  ; record_env  : (Ast.quantified_ty list) base_type_map
  ; variant_env : (Ast.quantified_ty base_type_map) base_type_map
  ; tyvar_env   : Ast.quantified_ty base_type_map
  } [@@ocamlformat "disable"]

(* utils *)
let drop_last l = List.take (List.length l - 1) l
let ( let* ) = Base.Or_error.( >>= )
let ( let@ ) = Base.Or_error.( >>| )

let flatten_arrow (arrow : Ast.located_ty) : Ast.located_ty list =
  let rec go acc = function
    | _, Ast.Arrow (l, r) -> go (l :: acc) r
    | t -> List.rev (t :: acc)
  in
  go [] arrow
;;

let rec build_arrow = function
  | [] ->
    raise (Error.InternalError "Internal error - there should be at least a return type.")
  | [ t ] -> t
  | ((loc', _) as h') :: t -> loc', Ast.Arrow (h', build_arrow t)
;;

(* we supply the location for more accurate type errors *)
let builtin_defs loc =
  let open Ast in
  [ (* dec print : string -> (). *)
    ["print"], Mono (loc, Arrow ((loc, Prim PString), (loc, Prim PUnit)))
  ; (* dec ( + | - | * | / ) : int -> int -> int. *)
    ["+"; "-"; "*"; "/"], Mono (loc, Arrow ((loc, Prim PInt), (loc, Arrow ((loc, Prim PInt), (loc, Prim PInt)))))
  ; (* dec ( +. | -. | *. | /. ) : float -> float -> float. *)
    ["+."; "-."; "*."; "/."], Mono (loc, Arrow ((loc, Prim PFloat), (loc, Arrow ((loc, Prim PFloat), (loc, Prim PFloat)))))
  ; (* dec ( < | > | <= | >= | = | /= ) : Uni 'a. 'a -> 'a -> bool. *)
    ["<"; ">"; "<="; ">="; "="; "/="], Uni (["'a"], (loc, Arrow ((loc, Prim (PGeneric "'a")), (loc, Arrow ((loc, Prim (PGeneric "'a")), (loc, Prim PBool))))))
  ; (* dec ( && | || ) : bool -> bool -> bool. *)
    ["&&"; "||"], Mono (loc, Arrow ((loc, Prim PBool), (loc, Arrow ((loc, Prim PBool), (loc, Prim PBool)))))
  ; (* dec ( :: ) : Uni 'a. 'a -> 'a list -> 'a list. *)
    ["::"], Uni (["'a"], (loc, Arrow ((loc, Prim (PGeneric "'a")), (loc, Arrow ((loc, List (loc, Prim (PGeneric "'a"))), (loc, List (loc, Prim (PGeneric "'a"))))))))
  ] [@@ocamlformat "disable"]

let find_def i loc =
  let rec go = function
    | [] -> None
    | (idents, ty) :: t ->
      (match List.filter (( = ) i) idents with
       | [] -> go t
       | _ -> Some ty)
  in
  go (builtin_defs loc)
;;

let fresh_tm () = TM.empty

let fresh_env () =
  { var_env = fresh_tm ()
  ; func_env = fresh_tm ()
  ; alias_env = fresh_tm ()
  ; tyvar_env = fresh_tm ()
  ; record_env = fresh_tm ()
  ; variant_env = fresh_tm ()
  }
;;

let fresh_tyvar =
  let i = ref (-1) in
  fun () ->
    incr i;
    "t_" ^ string_of_int !i
;;

let init_func_env (env : env) (defs : Ast.located_definition list) : env =
  let rec associate_funcs acc = function
    | [] -> acc
    | (h, ty) :: t ->
      let h = List.map (fun d -> d, ty) h in
      associate_funcs (h :: acc) t
  in
  { env with
    func_env =
      (builtin_defs Location.dummy_loc |> associate_funcs [] |> List.flatten)
      @ List.filter_map
          (function
            | _, Ast.Dec (_, i, t) -> Some (i, t)
            | _ -> None)
          defs
      |> TM.of_list
  }
;;

let init_type_env (env : env) (types : Ast.located_ty_decl list) : env =
  let alias, record, variant =
    ( List.filter_map (function | _, (i, Ast.Alias a) -> Some (i, a) | _ -> None) types |> TM.of_list
    , List.filter_map (function | _, (i, Ast.Record r) -> let r' = List.map (fun (_, l) -> l) r in Some (i, r') | _ -> None) types |> TM.of_list
    , List.filter_map (function | _, (i, Ast.Variant r) -> Some (i, TM.of_list r) | _ -> None) types |> TM.of_list )
  in
  { env with alias_env = alias; record_env = record; variant_env = variant; tyvar_env = TM.empty} [@@ocamlformat "disable"]

let get_value_type (value_env : 'a base_type_map) (i : string) : 'a option =
  TM.find_opt i value_env
;;

let value_exists (value_env : 'a base_type_map) (i : string) : bool = TM.mem i value_env

let add_value_type (value_env : 'a base_type_map) (i : string) (v : 'a) : 'a base_type_map
  =
  TM.add i v value_env
;;

(* used to remove any unresolved type variables in future code that have already been resolved *)
let replace_tyvars (env : env) (tenv : Ast.quantified_ty base_type_map) : env =
  { env with tyvar_env = tenv }
;;

let make_err (e : Error.t) : 'a Base.Or_error.t =
  Base.Or_error.error_string @@ Error.format_err e
;;

let rec unify
          (env : env)
          ~(location : Location.t)
          ~(expect : Ast.located_ty)
          ((loc', _) as r : Ast.located_ty)
  : env Base.Or_error.t
  =
  let open Ast in
  let open Base.Or_error in
  let ((loc, _) as l) = expect in
  let _, l = sub_ty env l in
  let _, r = sub_ty env r in
  match l, r with
  | Prim (PGeneric l), r -> bind_ty env l (loc', r)
  | l, Prim (PGeneric r) -> bind_ty env r (loc, l)
  | Udt ln, Udt rn when ln = rn -> Ok env
  | Prim l, Prim r when l = r -> Ok env
  | Ctor (ln, l), Ctor (rn, r) when ln = rn -> unify env ~location ~expect:l r
  | List l, List r -> unify env ~location ~expect:l r
  | Tuple ls, Tuple rs when List.length ls = List.length rs ->
    let e, wts =
      let rec go e acc l r =
        match l, r with
        | [], [] -> e, acc
        | h :: t, h' :: t' ->
          (match unify e ~location ~expect:h h' with
           | Ok e -> go e (Ok () :: acc) t t'
           | Error _ as e' -> go e (e' :: acc) t t')
        | _ ->
          raise
            (Error.InternalError
               "Internal error - called `go` within `unify` with mismatched list lengths.")
      in
      go env [] ls rs
    in
    let@ _ = combine_errors wts in
    replace_tyvars env e.tyvar_env
  | Arrow (l1, r1), Arrow (l2, r2) ->
    let* env = unify env ~location ~expect:l1 l2 in
    unify env ~location ~expect:r1 r2
  | _ ->
    make_err
      ( Some location
      , Printf.sprintf "Expected type %s, but got type %s." (show_ty l) (show_ty r) )

and sub_ty (env : env) ((p, t) : Ast.located_ty) : Ast.located_ty =
  let open Ast in
  match t with
  | Prim (PGeneric g) ->
    (match get_value_type env.tyvar_env g with
     | None -> p, t
     | Some ty -> instantiate ty |> sub_ty env)
  | List ty -> p, List (sub_ty env ty)
  | Tuple ts -> p, Tuple (List.map (fun t -> sub_ty env t) ts)
  | Arrow (l, r) -> p, Arrow (sub_ty env l, sub_ty env r)
  | Ctor (n, t) -> p, Ctor (n, sub_ty env t)
  | t -> p, t

and bind_ty (env : env) (n : string) ((p, t) : Ast.located_ty) : env Base.Or_error.t =
  let open Ast in
  let is_internal = String.starts_with ~prefix:"t_" in
  let rec occurs_within = function
    | _, Prim (PGeneric g) -> g = n (* we already know `n` is a tyvar *)
    | _, List t | _, Ctor (_, t) -> occurs_within t
    | _, Tuple ts -> List.fold_right (fun t acc -> occurs_within t || acc) ts false
    | _, Arrow (l, r) -> occurs_within l || occurs_within r
    | _ -> false
  in
  match t with
  | Prim (PGeneric g) when g = n -> Ok env
  | _ when occurs_within (p, t) && is_internal n ->
    make_err (Some p, Printf.sprintf "Found infinite type: %s." n)
  | _ -> Ok { env with tyvar_env = add_value_type env.tyvar_env n (Mono (p, t)) }

and instantiate (t : Ast.quantified_ty) : Ast.located_ty =
  let open Ast in
  let is_internal = String.starts_with ~prefix:"t_" in
  let (env : string TM.t) =
    (match t with
    | Mono _ -> []
    | Uni (gs, _) -> 
      List.map (fun i -> i, fresh_tyvar ()) gs)
    |> TM.of_list
  in
  let rec go env = function
    | (p, Prim (PGeneric g)) ->
      (match TM.find_opt g env with
      | Some g' -> (p, Prim (PGeneric g')), env
      | None ->
        let g' =
          if is_internal g
          then g
          else fresh_tyvar ()
        in
        let env = TM.add g g' env in
        (p, Prim (PGeneric g')), env)
    | p, List t ->
      let t, env = go env t in
      (p, List t), env
    | p, Ctor (n, t) ->
      let t, env = go env t in
     (p, Ctor (n, t)), env
    | p, Tuple ts ->
      let rec aux acc e = function
        | [] -> List.rev acc, e
        | h :: t ->
          let h, e = go e h in
          aux (h :: acc) e t
      in
      let ts, env = aux [] env ts in
      (p, Tuple ts), env
    | p, Arrow (l, r) ->
      let l, env = go env l in
      let r, env = go env r in
      (p, Arrow (l, r)), env
    | p, t -> (p, t), env
  in
  fst @@ go env (get_quantified_ty t)

and generalise (env: env) (ty: Ast.located_ty) : Ast.quantified_ty =
  let open Ast in
  let module S = Set.Make (String) in
  let rec collect_tyvars acc = function
    | _, Arrow (l, r) ->
      let acc = collect_tyvars acc l in
      let acc = collect_tyvars acc r in
      acc
    | _, List t -> collect_tyvars acc t
    | _, Ctor (_, t) -> collect_tyvars acc t
    | _, Tuple ts ->
      let rec go acc = function
        | [] -> acc
        | h :: t ->
          let acc = collect_tyvars acc h in
          go acc t
      in go acc ts
    | _, Prim (PGeneric g) -> S.add g acc
    | _ -> acc
  in
  let vars = sub_ty env ty |> collect_tyvars S.empty in
  match S.to_list vars with
  | [] -> Mono ty
  | vars -> Uni (vars, ty)
;;

let check_list
      ~(f : env -> 'a -> ('b * env) Base.Or_error.t)
      ~(rev : bool)
      (env : env)
      (l : 'a list)
  : 'b Base.Or_error.t list * env
  =
  let rec go acc e = function
    | [] when rev -> List.rev acc, e
    | [] -> acc, e
    | h :: t ->
      (match f e h with
       | Ok (h', e') -> go (Ok h' :: acc) e' t
       | Error _ as h' -> go (h' :: acc) e t)
  in
  go [] env l
;;

let check_list2
      ~(f : env -> 'a -> 'b -> ('c * env) Base.Or_error.t)
      ~(rev : bool)
      (env : env)
      (l : 'a list)
      (r : 'b list)
  : 'c Base.Or_error.t list * env
  =
  let rec go acc e l' r' =
    match l', r' with
    | [], [] when rev -> List.rev acc, e
    | [], [] -> acc, e
    | h :: t, h' :: t' ->
      (match f e h h' with
       | Ok (h'', e') -> go (Ok h'' :: acc) e' t t'
       | Error _ as h'' -> go (h'' :: acc) e t t')
    | _ ->
      raise
        (Error.InternalError
           "Internal error - called check_list2 with mismatched list lengths.")
  in
  go [] env l r
;;

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
    | Ident _ -> Prim (PGeneric (fresh_tyvar ()))
  in
  loc, t
;;

let rec check_type (env : env) ((loc, t) : Ast.located_ty) : unit Base.Or_error.t =
  let open Base.Or_error in
  match t with
  | Udt u
    when (not @@ value_exists env.alias_env u)
         && (not @@ value_exists env.record_env u)
         && (not @@ value_exists env.variant_env u) ->
    make_err (Some loc, Printf.sprintf "Undefined type %s." u)
  | Arrow (l, r) ->
    let* _ = check_type env l in
    check_type env r
  | List l -> check_type env l
  | Tuple t -> find_map_ok t ~f:(fun t' -> check_type env t')
  | Ctor (_, t') -> check_type env t'
  | _ -> Ok ()
;;

let rec get_pattern_type (env : env) ((loc, pat) : Ast.located_pattern)
  : (Ast.located_ty * env) Base.Or_error.t
  =
  let is_internal = String.starts_with ~prefix:"t_" in
  let open Ast in
  let open Base.Or_error in
  match pat with
  | PConst (_, Ident i) ->
    let t = loc, Prim (PGeneric (fresh_tyvar ())) in
    let env' = { env with var_env = add_value_type env.var_env i (Mono t) } in
    Ok (t, env')
  | PConst c -> Ok (get_const_type c, env)
  | PList l ->
    (match l with
     | [] ->
        let t = loc, Prim (PGeneric (fresh_tyvar ())) in
        Ok ((loc, List t), env)
     | items ->
       let ty_list, env' = check_list ~f:get_pattern_type ~rev:true env items in
       let* l' = combine_errors ty_list in
       let non_generic =
         List.find_opt
           (function
             | _, Prim (PGeneric _) -> false
             | _ -> true)
           l'
       in
       (match non_generic with
        | None -> 
          let t = loc, Prim (PGeneric (fresh_tyvar ())) in
          Ok ((loc, List t), env')
        | Some ((type_loc, _) as t) ->
          let rec go acc e = function
            | [] -> List.rev acc, e
            | h :: t' ->
              (match unify e ~location:loc ~expect:t h with
               | Ok e -> go (Ok () :: acc) e t'
               | Error _ as err -> go (err :: acc) e t')
          in
          let wts, env = go [] env l' in
          let@ _ = combine_errors wts in
          (type_loc, List t), env))
  | PTup t ->
    let ty_list, env' = check_list ~f:get_pattern_type ~rev:true env t in
    let@ t' = combine_errors ty_list in
    (loc, Tuple t'), env'
  | PCons (l, r) ->
    let* (loc1, l'), e = get_pattern_type env l in
    let* (loc2, r'), env' = get_pattern_type e r in
    let loc' = Location.combine loc1 loc2 in
    (match r' with
     | Prim (PGeneric g) when is_internal g -> Ok ((loc', List (loc', l')), env')
     | List t' as t ->
       let@ env = unify env ~location:loc ~expect:t' (loc1, l') in
       (loc', t), env
     | t ->
       make_err
         ( Some loc'
         , Printf.sprintf "Expected a list type, but got type %s instead." (show_ty t) ))
  | PWild -> Ok ((loc, Prim (PGeneric (fresh_tyvar ()))), env)
  | PCtor (i, v) ->
    let* v, env = get_pattern_type env v in
    let rec aux l =
      match l with
      | [] -> make_err (Some loc, Printf.sprintf "Undefined constructor - %s." i)
      | (_, vs) :: rest ->
        (match get_value_type vs i with
        | None -> aux rest
        | Some t -> 
          let t = instantiate t |> sub_ty env |> flatten_arrow in
          (*NOTE: this'll likely cause issues with variants that contain function signatures.*)
          let@ env = unify env ~location:loc ~expect:(List.hd t) v in
          (* we can safely use the _exn version as every constructor will return the overarching type *)
          Base.List.last_exn t, env)
    in
    aux @@ TM.to_list env.variant_env
;;

let rec check_expr (env : env) (e : Ast.located_expr)
  : (Typed_ast.typed_expr * env) Base.Or_error.t
  =
  let@ e, env = check_expr' env e in
  sub_expr env e, env

and sub_expr (env : env) ((t, (p, e)) : Typed_ast.typed_expr) : Typed_ast.typed_expr =
  let open Typed_ast in
  let t = sub_ty env t in
  match e with
  | Const _ -> t, (p, e)
  | EList es -> t, (p, EList (List.map (fun e -> sub_expr env e) es))
  | ETup es -> t, (p, ETup (List.map (fun e -> sub_expr env e) es))
  | Bop (l, op, r) -> t, (p, Bop (sub_expr env l, op, sub_expr env r))
  | Ap (b, l, r) -> t, (p, Ap (b, sub_expr env l, sub_expr env r))
  | Let (pat, v, n) -> t, (p, Let (pat, sub_expr env v, sub_expr env n))
  | Grouping ts -> t, (p, Grouping (sub_expr env ts))
  | If (c, tr, f) ->
    let f = Base.Option.map f ~f:(fun f' -> sub_expr env f') in
    t, (p, If (sub_expr env c, sub_expr env tr, f))
  | Lam (ps, v) -> t, (p, Lam (ps, sub_expr env v))
  | Match (e, bs) ->
    let e = sub_expr env e in
    let bs = List.map (fun (p, wb, e) -> let wb = Base.Option.map wb ~f:(sub_expr env) in p, wb, sub_expr env e) bs in
    t, (p, Match (e, bs))

and check_expr' (env : env) ((loc, e) : Ast.located_expr)
  : (Typed_ast.typed_expr * env) Base.Or_error.t
  =
  let open Ast in
  let open Base.Or_error in
  let rec get_type: Typed_ast.typed_expr -> Ast.located_ty = function
    | _, (_, Let (_, _, n)) -> get_type n
    | _, (_, Grouping n) -> get_type n
    | typ, _ -> typ
  in
  match e with
  | EList es ->
    let es', env = check_list ~f:check_expr ~rev:true env es in
    let* exprs = combine_errors es' in
    (match exprs with
     | [] -> 
        let t = loc, Prim (PGeneric (fresh_tyvar ())) in
        Ok (((loc, List t), (loc, Typed_ast.EList [])), env)
     | (((ty_loc, _) as t), _) :: tl ->
       let rec go e acc = function
         | [] -> acc, e
         | (h, _) :: t' ->
           (match unify e ~location:loc ~expect:t h with
           | Ok e -> go e (Ok () :: acc) t'
           | Error _ as err -> go e (err :: acc) t')
       in
       let wts, env = go env [] tl in
       let@ _ = combine_errors wts in
       ((ty_loc, List t), (loc, Typed_ast.EList exprs)), env)
  | Const (s, Ident i) ->
    (match get_value_type env.var_env i with
     | None ->
       (match get_value_type env.func_env i with
       | None ->
         let rec aux l =
           match l with
           | [] -> make_err (Some loc, Printf.sprintf "Undefined identifier - %s." i)
           | (_, vs) :: rest ->
             (match get_value_type vs i with
              | None -> aux rest
              | Some t -> Ok ((instantiate t |> sub_ty env, (loc, Typed_ast.Const (s, Ident i))), env))
         in
         aux @@ TM.to_list env.variant_env
      | Some t -> 
        Ok ((instantiate t |> sub_ty env, (loc, Typed_ast.Const (s, Ident i))), env))
     | Some t -> Ok ((instantiate t |> sub_ty env, (loc, Typed_ast.Const (s, Ident i))), env))
  | Const c -> Ok ((get_const_type c, (loc, Typed_ast.Const c)), env)
  | ETup contents ->
    let contents, env = check_list env ~f:check_expr ~rev:true contents in
    let@ values = combine_errors contents in
    let ts = List.map (fun (t, _) -> t) values in
    let tup = loc, Ast.Tuple ts in
    (tup, (loc, Typed_ast.ETup values)), env
  | Ap (b, l, r) ->
    let get_right = function
      | _, Arrow (_, r') -> r'
      | t' -> t'
    in
    let* (t, _) as l', env = check_expr env l in
    let* (t', _) as r', env = check_expr env r in
    let open Rename in
    let lt =
      if b > Alpha.user_bind (* checking if it's builtin or not *)
      then t
      else (
        let i = Alpha.find_ident (loc, e) in
        match find_def i loc with
        | Some bt -> instantiate bt
        | None -> raise (Error.InternalError "Internal error - improper binder."))
    in
    let left_conn = List.hd (flatten_arrow lt)
    and right_conn = List.rev (flatten_arrow t') |> List.hd
    and ret = get_right lt in
    let@ env = unify env ~location:loc ~expect:left_conn right_conn in
    (ret, (loc, Typed_ast.Ap (b, l', r'))), env
  | Bop (l, op, r) ->
    let* ((ty_loc, t) as lt, _) as l', e  =check_expr env l in
    let env = replace_tyvars env e.tyvar_env in
    let* ((ty_loc', t') as rt, re) as r', e = check_expr env r in
    let env = replace_tyvars env e.tyvar_env in
    (match op with
     | (IAdd | IMul | IDiv | ISub) ->
       let* env = unify env ~location:loc ~expect:(ty_loc, Prim PInt) lt in
       let@ env =  unify env ~location:loc ~expect:(ty_loc, Prim PInt) rt in
       ((ty_loc', Prim PInt), (loc, Typed_ast.Bop (l', op, r'))), env
     | (FAdd | FMul | FDiv | FSub) ->
       let* env = unify env ~location:loc ~expect:(ty_loc, Prim PFloat) lt in
       let@ env = unify env ~location:loc ~expect:(ty_loc, Prim PFloat) rt in
       ((ty_loc', Prim PFloat), (loc, Typed_ast.Bop (l', op, r'))), env
     | (And | Or) ->
       let* env = unify env ~location:loc ~expect:(ty_loc, Prim PBool) lt in
       let@ env = unify env ~location:loc ~expect:(ty_loc', Prim PBool) rt in
       ((ty_loc, t), (loc, Typed_ast.Bop (l', op, r'))), env
     | (Less | Greater | LessE | GreaterE | Equal | NotEq) ->
       let@ env = unify env ~location:loc ~expect:lt rt in
       ((ty_loc, Prim PBool), (loc, Typed_ast.Bop (l', op, r'))), env
     | Cons ->
       (match t' with
        | List list_type ->
          let@ env = unify env ~location:loc ~expect:lt list_type in
          ((ty_loc, t'), (loc, Typed_ast.Bop (l', op, r'))), env
        | Prim (PGeneric g) when String.starts_with ~prefix:"t_" g ->
          let list_type = ty_loc', List (ty_loc', t) in
          Ok (((ty_loc, List (ty_loc, t)), (loc, Typed_ast.Bop (l', op, (list_type, re)))), env)
        | err_type ->
          make_err
            ( Some loc
            , Printf.sprintf
                "Expected an empty list or a list type, but got %s."
                (show_ty err_type) ))
     | User_op i ->
       (match get_value_type env.func_env i with
        | None -> make_err (Some loc, Printf.sprintf "Undefined operator - %s." i)
        | Some fsig ->
          let (_, esig) as fsig = instantiate fsig in
          (match flatten_arrow fsig with
           | lt' :: rt' :: ret ->
             let* env = unify env ~location:loc ~expect:lt' lt in
             let@ env = unify env ~location:loc ~expect:rt' rt in
             (build_arrow ret, (loc, Typed_ast.Bop (l', op, r'))), env
           | _ ->
             make_err
               ( Some loc
               , Printf.sprintf
                   "Expected binary operator, but got operator with signature %s."
                   (show_ty esig) ))))
  | Let (p, typ, v, n) ->
    let* ((t', _) as v), e = check_expr env v in
    let env = replace_tyvars env e.tyvar_env in
    (match typ with
     | Some typ ->
       let* _ = check_type env (get_quantified_ty typ) in
       let typ = instantiate typ in
       let* env = 
          let* env = unify env ~location:loc ~expect:typ t' in
          let* p, env = get_pattern_type env p in
          unify env ~location:loc ~expect:typ p
       in
       let@ n, env = check_expr env n in
       (typ, (loc, Typed_ast.Let (p, v, n))), env
     | None ->
       let* env =
         let* p, env = get_pattern_type env p in
         unify env ~location:loc ~expect:t' p
       in
       let@ n, env = check_expr env n in
       (get_type n, (loc, Typed_ast.Let (p, v, n))), env)
  | Grouping ts ->
    let@ ts, e = check_expr env ts in
    let env = replace_tyvars env e.tyvar_env in
    (get_type ts, (loc, Typed_ast.Grouping ts)), env
  | If (cond, texpr, fexpr) ->
    let* ((((ty_loc, _) as ty), _) as cond), e = check_expr env cond in
    let env = replace_tyvars env e.tyvar_env in
    let* env = unify env ~location:loc ~expect:(ty_loc, Prim PBool) ty in
    let* ((((_, t) as tty), _) as texpr), e = check_expr env texpr in
    let env = replace_tyvars env e.tyvar_env in
    (match fexpr with
     | None -> Ok (((loc, t), (loc, Typed_ast.If (cond, texpr, None))), env)
     | Some fexpr ->
       let* ((fty, _) as fexpr), e = check_expr env fexpr in
       let env = replace_tyvars env e.tyvar_env in
       let@ env = unify env ~location:loc ~expect:tty fty in
       ((loc, t), (loc, Typed_ast.If (cond, texpr, Some fexpr))), env)
  | Lam (args, body) ->
    let x, env = check_list ~f:get_pattern_type ~rev:false env args in
    let* tys = combine_errors x in
    let@ ((ret, _) as body), e = check_expr env body in
    let env = replace_tyvars env e.tyvar_env in
    let ty = ret :: tys |> List.rev |> build_arrow |> sub_ty env in
    (ty, (loc, Typed_ast.Lam (args, body))), env
  | Match (e, bs) ->
    let* (ty, _) as e, env' = check_expr env e in
    let rec go env' acc = function
      | [] -> List.rev acc, replace_tyvars env env'.tyvar_env
      | ((loc, _) as p, wb, e) :: t ->
        let v =
          let* p', env = get_pattern_type env' p in
          let* env = unify env ~location:loc ~expect:ty p' in
          let* wb, env = 
            match wb with
            | None -> Ok (None, env)
            | Some wb ->
              let* (wb_ty, ((wb_loc, _) as wb)), e = check_expr env wb in
              let env = replace_tyvars env e.tyvar_env in
              let@ env = unify env ~location:wb_loc ~expect:(wb_loc, Prim PBool) wb_ty in
              Some ((wb_loc, Prim PBool), wb), env
          in
          let@ e, env = check_expr env e in
          (p, wb, e), env
        in
        (match v with
        | Ok (br, env) -> go env (Ok br :: acc) t
        | Error _ as err -> go env' (err :: acc) t)
    in
    let@ bs, env, ret = 
      let bs, env = go env' [] bs in
      let* bs = combine_errors bs in
      let uniform_typing branches =
        match branches with
        | [] -> raise (Error.InternalError "Internal error - there must be at least one branch in a match expression.")
        | (_, _, expr) :: t ->
          let ty = get_type expr in
          let rec go e acc = function
            | [] -> acc, e
            | (_, _, (vt, (loc, _))) :: t ->
              (match unify e ~location:loc ~expect:ty vt with
              | Ok e -> go e (Ok () :: acc) t
              | Error _ as err -> go e (err :: acc) t)
          in
          let errs, env = go env [] t in
          let@ _ = combine_errors errs in
          branches, env, ty
      in uniform_typing bs
    in
    (ret, (loc, Typed_ast.Match (e, bs))), env
[@@ocamlformat "disable"]

let rec check_def (env : env) (loc, (hsd, i, args, when_block, body, with_block))
  : (Typed_ast.located_definition list * env) Base.Or_error.t
  =
  let open Ast in
  let open Base.Or_error in
  let func_type = get_value_type env.func_env i in
  let* with_block', e =
    match with_block with
    | [] -> Ok ([], env)
    | ds ->
      let rec go acc = function
        | [] -> acc
        | (_, Dec (_, i, t)) :: tl -> go (TM.add i t acc) tl
        | _ :: tl -> go acc tl
      in
      let env = { env with func_env = go env.func_env ds } in
      let ds =
        List.filter_map
          (function
            | _, Dec _ -> None
            | loc', Def (hsd, i', args', when_block', body', with_block') ->
              Some (loc', (hsd, i', args', when_block', body', with_block')))
          ds
      in
      let ds, env = check_list ~f:check_def ~rev:true env ds in
      let@ defs = combine_errors ds in
      List.flatten defs, env
  in
  let (args', env), eta_extra =
    match func_type with
    | Some dec_ty ->
      let dec_ty = instantiate dec_ty in
      Printf.printf "%s dec_ty: %s\n" i (snd dec_ty |> show_ty);
      let t = flatten_arrow dec_ty |> drop_last in
      (match () with
      (* kinda hacky, but whatever *)
      | _ when List.length t < List.length args -> ([make_err (Some loc, Printf.sprintf "The function %s expects %i arguments, but %i arguments were found." i (List.length t) (List.length args))], env), []
      | _ -> 
        let unify_arg env sig_ty pat =
          let* pat_ty, env = get_pattern_type env pat in
          let@ env = unify env ~location:loc ~expect:sig_ty pat_ty in
          sig_ty, env
        in
        let n = List.length args in
        (* we compare our given pattern types to any given types. any excess is carried on - η-reduction *)
        let t, ret = List.take n t, List.drop n t in
        check_list2 ~f:unify_arg ~rev:false e t args, ret)
    | None ->
      Error.report_warning
        ( Some loc
        , Printf.sprintf "Top level definition '%s' lacks accompanying type signature." i
        );
      check_list ~f:get_pattern_type ~rev:false e args, []
  in
  let* typed_args = combine_errors args' in
  let* wb, env =
    match when_block with
    | None -> Ok (None, env)
    | Some wb ->
      let* (((type_loc, _) as ty), wb), e = check_expr env wb in
      let env = replace_tyvars env e.tyvar_env in
      let@ _ = unify env ~location:loc ~expect:(type_loc, Prim PBool) ty in
      Some ((type_loc, Prim PBool), wb), env
  in
  let* body, e = check_expr env body in
  let env = replace_tyvars env e.tyvar_env in
  let last =
    let rec get_type : Typed_ast.typed_expr -> Ast.located_ty = function
      | _, (_, Let (_, _, n)) -> get_type n
      | _, (_, Grouping n) -> get_type n
      | typ, _ -> typ
    in
    let l = get_type body |> flatten_arrow in
    (* we drop any eta reduced types from the inferred type of the body. if there are none, it has no effect. *)
    (List.drop (List.length eta_extra) l |> build_arrow) :: eta_extra
  in
  let ret = last @ typed_args |> List.rev |> build_arrow |> sub_ty env in
  match func_type with
  | None ->
    let ret = generalise env ret in
    let env = { env with func_env = add_value_type env.func_env i ret } in
    let d = loc, (hsd, i, ret, args, wb, body) in
    Ok (d :: with_block', env)
  | Some dec_ty ->
    let dec_ty = instantiate dec_ty in
    let@ env = unify env ~location:loc ~expect:dec_ty ret in
    let d = loc, (hsd, i, (generalise env dec_ty), args, wb, body) in
    d :: with_block', env
;;

let check_program
      ?should_check_main:(chk_main = true)
      ((n, imp, typs, defs) : Ast.program)
  : Typed_ast.program Base.Or_error.t
  =
  let open Base.Or_error in
  let env = init_func_env (fresh_env ()) defs |> Fun.flip init_type_env typs in
  let decs =
    List.filter_map
      (function
        | _, Ast.Def _ -> None
        | _, Ast.Dec (_, i, dec) -> Some (i, dec))
      defs
  in
  let defs =
    List.filter_map
      (function
        | _, Ast.Dec _ -> None
        | loc, Ast.Def (hsd, i, args, when_block, body, with_block) ->
          Some (loc, (hsd, i, args, when_block, body, with_block)))
      defs
  in
  let* _ = List.map (fun (_, t) -> match t with | Ast.Uni (_, t) | Ast.Mono t -> check_type env t) decs |> combine_errors in
  let mains =
    List.filter_map
      (function
        | _, (_, "main", _, _, _, _) -> Some ()
        | _ -> None)
      defs
  in
  let* _ =
    match mains with
    | [] when chk_main -> make_err (None, "Expected entrypoint.")
    | [ _ ] -> Ok ()
    | _ when chk_main -> make_err (None, "Multiple entrypoints found.")
    | _ -> Ok ()
  in
  let@ typed_defs = check_list ~f:check_def ~rev:true env defs |> fst |> combine_errors in
  n, imp, typs, List.flatten typed_defs
;;
