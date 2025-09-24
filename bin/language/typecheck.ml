open Util

type 'a base_env = (string * 'a) list

type env =
  { func_env : Ast.located_ty base_env
  ; var_env : Ast.located_ty base_env
  ; alias_env : Ast.located_ty base_env
  ; record_env : Ast.located_ty list base_env
  ; variant_env : Ast.located_ty option base_env base_env
  }

(* utils *)
let drop_last l =
  let rec go acc = function
    | [] ->
      raise (Error.InternalError "Internal error - called drop_last with empty list.")
    | [ _ ] -> List.rev acc
    | h :: t -> go (h :: acc) t
  in
  go [] l
;;

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

(* custom equality function for types that respects type variables *)
let rec ( &= ) (l : Ast.ty) (r : Ast.ty) : bool =
  let open Ast in
  let is_internal = String.starts_with ~prefix:"t_" in
  match l, r with
  | Prim (PGeneric lg), Prim (PGeneric rg)
    when (not @@ is_internal lg) && (not @@ is_internal rg) -> lg = rg
  | Prim (PGeneric _), _ -> true
  | _, Prim (PGeneric _) -> true
  | Prim l', Prim r' -> l' = r'
  | Tuple l', Tuple r' when List.length l' = List.length r' ->
    List.fold_left2 (fun acc (_, t1) (_, t2) -> acc && t1 &= t2) true l' r'
  | List (_, l'), List (_, r') -> l' &= r'
  | Udt l', Udt r' -> l' = r'
  | Arrow ((_, l1'), (_, r1')), Arrow ((_, l2'), (_, r2')) -> l1' &= l2' && r1' &= r2'
  | _ -> false
;;

let ( &!= ) l r : bool = not (l &= r)

let builtin_defs loc =
  [ (* dec print : string -> (). *)
    ( "print"
    , ( loc
      , Ast.Arrow
          ( (loc, Ast.Prim Ast.PString)
          , (loc, Ast.Prim Ast.PUnit) ) ) )
  ]
;;

let fresh_env () =
  { var_env = []; func_env = []; alias_env = []; record_env = []; variant_env = [] }
;;

let fresh_tyvar =
  let i = ref (-1) in
  fun () ->
    incr i;
    "t_" ^ string_of_int !i
;;

let init_func_env (env : env) (defs : Ast.located_definition list) : env =
  { env with
    func_env =
      builtin_defs Location.dummy_loc
      @ List.filter_map
          (function
            | _, Ast.Dec (i, t) -> Some (i, t)
            | _ -> None)
          defs
  }
;;

let init_type_env (env : env) (types : Ast.located_ty_decl list) : env =
  let alias, record, variant =
    ( List.filter_map
        (function
          | _, (i, Ast.Alias a) -> Some (i, a)
          | _ -> None)
        types
    , List.filter_map
        (function
          | _, (i, Ast.Record r) ->
            let r' = List.map (fun (_, l) -> l) r in
            Some (i, r')
          | _ -> None)
        types
    , List.filter_map
        (function
          | _, (i, Ast.Variant r) -> Some (i, r)
          | _ -> None)
        types )
  in
  { env with alias_env = alias; record_env = record; variant_env = variant }
;;

let combine_env (l : env) (r : env) : env =
  { func_env = l.func_env @ r.func_env
  ; var_env = l.var_env @ r.var_env
  ; alias_env = l.alias_env @ r.alias_env
  ; record_env = l.record_env @ r.record_env
  ; variant_env = l.variant_env @ r.variant_env
  }
;;

let get_value_type (value_env : 'a base_env) (i : string) : 'a option =
  List.assoc_opt i value_env
;;

let value_exists (value_env : 'a base_env) (i : string) : bool =
  List.mem_assoc i value_env
;;

let add_var_type (env : env) (var : string) (ty : Ast.located_ty) : env =
  { env with var_env = (var, ty) :: env.var_env }
;;

let replace_var_type (env: env) (var: string) (ty: Ast.located_ty) : env =
  let var_env' = List.remove_assoc var env.var_env in
  add_var_type {env with var_env=var_env'} var ty

let add_func_type (env : env) (func : string) (ty : Ast.located_ty) : env =
  { env with func_env = (func, ty) :: env.func_env }
;;

let make_err (e : Error.t) : 'a Base.Or_error.t =
  Base.Or_error.error_string @@ Error.format_err e
;;

(* main stuff *)
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

let rec check_type (env : env) ((loc, t) : Ast.located_ty) : unit Base.Or_error.t =
  let open Base.Or_error in
  match t with
  | Udt u
    when (not @@ value_exists env.alias_env u)
         || (not @@ value_exists env.record_env u)
         || (not @@ value_exists env.variant_env u) ->
    make_err (Some loc, Printf.sprintf "Undefined type %s." u)
  | Arrow (l, r) -> check_type env l >>= fun _ -> check_type env r
  | List l -> check_type env l
  | Tuple t -> find_map_ok t ~f:(fun t' -> check_type env t')
  | Constructor (_, t') -> check_type env t'
  | _ -> Ok ()
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
  | PList l ->
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
  | PTup t ->
    let ty_list, env' = check_list ~f:get_pattern_type ~rev:true env t in
    combine_errors ty_list >>| fun t' -> (loc, Tuple t'), env'
  | PCons (l, r) ->
    get_pattern_type env l
    >>= fun ((loc1, l'), e) ->
    get_pattern_type e r
    >>= fun ((loc2, r'), env') ->
    let loc' = Location.combine loc1 loc2 in
    (match r' with
     | Prim (PGeneric g) when is_internal g -> Ok ((loc', List (loc', l')), env')
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
    (match get_value_type env.var_env i with
     | None ->
       let rec aux l =
         match l with
         | [] -> make_err (Some loc, Printf.sprintf "Undefined identifier - %s." i)
         | (udt, vs) :: rest ->
           (match get_value_type vs i with
            | None ->
              (match get_value_type env.func_env i with
               | None -> aux rest
               | Some t' -> Ok (t', (loc, Typed_ast.Ident i)))
            | Some None -> Ok ((loc, Udt udt), (loc, Typed_ast.Ident i))
            | Some (Some t') -> Ok (t', (loc, Typed_ast.Ident i)))
       in
       aux env.variant_env
     | Some t -> Ok (t, (loc, Typed_ast.Ident i)))
  | ETup contents ->
    List.map (fun e -> check_expr env e) contents
    |> combine_errors
    >>= fun values ->
    let tup_types = List.map (fun (t, _) -> t) values in
    let tup = loc, Ast.Tuple tup_types in
    Ok (tup, (loc, Typed_ast.ETup values))
  | Ap (b, l, r) ->
    check_expr env l
    >>= fun ((t, _) as l') ->
    check_expr env r
    >>= fun ((t', _) as r') ->
    let open Rename in
    let lt = 
      if (b > Alpha.user_bind) (* checking if it's builtin or not *)
      then t
      else
        let i = Alpha.find_ident (loc, e) in
        match List.assoc_opt i (builtin_defs loc) with
        | Some bt -> bt
        | None -> raise (Error.InternalError "Internal error - improper binder.")
    in
    let ft = flatten_arrow lt in
    let _, left_conn = List.hd ft
    and _, right_conn = List.rev (flatten_arrow t') |> List.hd
    and ret = List.rev ft |> List.hd in
    if left_conn &= right_conn
    then Ok (ret, (loc, Typed_ast.Ap (b, l', r')))
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
       else (
         match t with
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
       (match get_value_type env.func_env i with
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
  | TLet (i, _, _) when value_exists env.var_env i || value_exists env.func_env i ->
    (*TODO: just replace the type instead & warn about shadowing *)
    make_err (Some loc, Printf.sprintf "Variable already defined - '%s'." i)
  | TLet (i, typ, v) ->
    check_term env v
    >>= fun ((((type_loc, t'), _) as v'), _) ->
    (match typ with
     | Some ((loc', typ') as typ'') ->
       check_type env typ''
       >>= fun _ ->
       if typ' &= t'
       then (
         let env' = 
          if value_exists env.var_env i 
          then replace_var_type env i (loc', typ')
          else add_var_type env i (loc', typ')
         in
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
  | TLam (args, body) ->
    let x, env' = check_list ~f:get_pattern_type ~rev:false env args in
    combine_errors x
    >>= fun tys ->
    check_term env' body
    >>= fun (((ret, _) as body'), _) ->
    let tys' = ret :: tys |> List.rev |> build_arrow in
    Ok ((tys', (loc, Typed_ast.TLam (args, body'))), env)
;;

let rec check_def (env : env) (loc, (i, args, when_block, body, with_block))
  : (Typed_ast.located_definition list * env) Base.Or_error.t
  =
  let open Ast in
  let open Base.Or_error in
  let func_type = get_value_type env.func_env i in
  let wb_res =
    match with_block with
    | None -> Ok ([], env)
    | Some ds ->
      let ds' =
        List.filter_map
          (function
            | _, Dec _ -> None
            | loc', Def (i', args', when_block', body', with_block') ->
              Some (loc', (i', args', when_block', body', with_block')))
          ds
      in
      let env' = init_func_env (fresh_env ()) ds |> combine_env env in
      let ds'', env'' = check_list ~f:check_def ~rev:true env' ds' in
      combine_errors ds'' >>| fun defs -> List.flatten defs, env''
  in
  wb_res
  >>= fun (with_block', e) ->
  let args', env' =
    match func_type with
    | None -> 
      Error.report_warning
        (Some loc, "Top level definition lacks accompanying type signature.");
      check_list ~f:get_pattern_type ~rev:false e args
    | Some dec_ty ->
      let rec unify_arg
                (e : env)
                ((ty_loc, l) : Ast.located_ty)
                ((_, r) : Ast.located_pattern)
        : (Ast.located_ty * env) Base.Or_error.t
        =
        match r with
        | PIdent i ->
          let e' = add_var_type e i (ty_loc, l) in
          Ok ((ty_loc, l), e')
        | PConst c ->
          let loc', c' = get_const_type c in
          if l &= c'
          then Ok ((ty_loc, l), e)
          else
            make_err
              ( Some loc'
              , Printf.sprintf
                  "Expected type %s, but got type %s."
                  (show_ty l)
                  (show_ty c') )
        | PWild -> Ok ((ty_loc, l), e)
        | PCons (l', r') ->
          (match l with
           | List t ->
             unify_arg e (ty_loc, l) r'
             >>= fun (_, e') -> unify_arg e' t l' >>| fun (_, e'') -> (ty_loc, l), e''
           | _ ->
             make_err
               ( Some ty_loc
               , Printf.sprintf "Expected list type, but got type %s." (show_ty l) ))
        | PList items ->
          (match l with
           | List t ->
             let l', e' =
               check_list ~f:(fun e'' v -> unify_arg e'' t v) ~rev:true e items
             in
             combine_errors l' >>| fun _ -> (ty_loc, l), e'
           | _ ->
             make_err
               ( Some ty_loc
               , Printf.sprintf "Expected a list type, but got type %s." (show_ty l) ))
        | PTup items ->
          (match l with
           | Tuple ts when List.length items = List.length ts ->
             let items', e' = check_list2 ~f:unify_arg ~rev:true env ts items in
             combine_errors items' >>| fun _ -> (ty_loc, l), e'
           | _ ->
             make_err
               ( Some ty_loc
               , Printf.sprintf "Expected a tuple type, but got type %s." (show_ty l) ))
      in
      let t' = flatten_arrow dec_ty |> drop_last in
      check_list2 ~f:unify_arg ~rev:false e t' args
  in
  combine_errors args'
  >>= fun typed_args ->
  (match when_block with
   | None -> Ok None
   | Some wb ->
     check_term env' wb
     >>= fun (((type_loc, wb_type), wb'), _) ->
     if wb_type &= Prim PBool
     then Ok (Some ((type_loc, Prim PBool), wb'))
     else
       make_err
         ( Some type_loc
         , Printf.sprintf "Expected type bool, but got type %s." (show_ty wb_type) ))
  >>= fun wb ->
  let body', _ = check_list ~f:check_term ~rev:true env' body in
  combine_errors body'
  >>= fun typed_body ->
  let (last_loc, last), _ = List.rev typed_body |> List.hd in
  let ret_loc, ret = (last_loc, last) :: typed_args |> List.rev |> build_arrow in
  match func_type with
  | None ->
    let env'' = add_func_type env i (ret_loc, ret) in
    let d = loc, (i, (ret_loc, ret), args, wb, typed_body) in
    Ok (d :: with_block', env'')
  | Some ((_, t) as dec_ty) when t &= ret ->
    check_type env' dec_ty
    >>= fun _ ->
    let d = loc, (i, dec_ty, args, wb, typed_body) in
    Ok (d :: with_block', env')
  | Some (_, t) ->
    make_err
      ( Some loc
      , Printf.sprintf "Expected type %s, but got type %s." (show_ty t) (show_ty ret) )
;;

let check_program ((n, imp, typs, defs) : Ast.program) : Typed_ast.program Base.Or_error.t
  =
  let open Base.Or_error in
  let env = init_func_env (fresh_env ()) defs |> Fun.flip init_type_env typs in
  let defs' =
    List.filter_map
      (function
        | _, Ast.Dec _ -> None
        | loc, Ast.Def (i, args, when_block, body, with_block) ->
          Some (loc, (i, args, when_block, body, with_block)))
      defs
  in
  let decs =
    List.filter_map
      (function
        | _, Ast.Def _ -> None
        | _, Ast.Dec (i, dec) -> Some (i, dec))
      defs
  in
  List.map (fun (_, t) -> check_type env t) decs
  |> combine_errors
  >>= fun _ ->
  let mains =
    List.filter_map
      (function
        | _, ("main", _, _, _, _) -> Some ()
        | _ -> None)
      defs'
  in
  (match mains with
   | [] -> make_err (None, "Expected entrypoint.")
   | [ _ ] -> Ok ()
   | _ -> make_err (None, "Multiple entrypoints found."))
  >>= fun _ ->
  check_list ~f:check_def ~rev:true env defs'
  |> fst
  |> combine_errors
  >>| fun typed_defs -> n, imp, typs, List.flatten typed_defs
;;
