open Primitive
open Elab

(* alpha renaming *)
module AR : sig
  val user_bind : int
  val find_ident : located_expr -> string
  val fresh_alpha : string -> ident
  val rename_program : program -> program
end = struct
  module VM = Map.Make (String)

  (* any binder above this value is user defined *)
  let user_bind = 17

  let builtins =
    [ "print", 0
    ; "+", 1
    ; "+.", 2
    ; "-", 3
    ; "-.", 4
    ; "*", 5
    ; "*.", 6
    ; "/", 7
    ; "/.", 8
    ; "<", 9
    ; ">", 10
    ; "<=", 11
    ; ">=", 12
    ; "=", 13
    ; "/=", 14
    ; "&&", 15
    ; "||", 16
    ; "::", 17
    ]
  ;;

  let fresh_binder =
    let i = ref user_bind in
    fun () ->
      incr i;
      !i
  ;;

  let fresh_alpha =
    let i = ref (-1) in
    fun s ->
      incr i;
      GStr (s, !i)
  ;;

  let fresh_env () = VM.empty

  let rename_list ~(f : 'b VM.t -> 'a -> 'a * 'b VM.t) (env : 'b VM.t) (l : 'a list)
    : 'a list * 'b VM.t
    =
    let (e, v) : 'b VM.t * 'a list =
      List.fold_left_map
        (fun e v ->
           let v, e = f e v in
           e, v)
        env
        l
    in
    v, e
  ;;

  let find_ident ((_, e) : located_expr) : string =
    match e with
    | Const (_, Ident i) -> get_str_combine i
    | Const (_, AccessIdent i) -> List.rev i |> List.hd |> get_str_combine
    | Const (_, Udc i) -> get_str_combine i
    | Binding (i, _, _) -> get_str_combine i
    | _ -> ""
  ;;

  let rec rename_pattern (env : ident VM.t) ((loc, pat) : located_pattern)
    : located_pattern * ident VM.t
    =
    match pat with
    | PConst (s, Ident i) when not @@ String.starts_with ~prefix:"__match__" (get_str i)
      ->
      let i = get_str i in
      let i' = fresh_alpha i in
      let env = VM.add i i' env in
      (loc, PConst (s, Ident i')), env
    | PConst (s, AccessIdent is) ->
      let is = List.map get_str is in
      let is' = List.map fresh_alpha is in
      let env =
        List.fold_left2
          (fun env i i' ->
             let env = VM.add i i' env in
             env)
          env
          is
          is'
      in
      (loc, PConst (s, AccessIdent is')), env
    | PConst _ -> (loc, pat), env
    | PWild -> (loc, pat), env
    | PBop (l, op, r) ->
      let l, env = rename_pattern env l in
      let r, env = rename_pattern env r in
      (loc, PBop (l, op, r)), env
    | PCtor (i, v) ->
      let i =
        match VM.find_opt (get_str i) env with
        | Some i' -> i'
        | None -> i
      in
      let v, env = rename_list ~f:rename_pattern env v in
      (loc, PCtor (i, v)), env
    | PTuple ps ->
      let ps, env = rename_list ~f:rename_pattern env ps in
      (loc, PTuple ps), env
  ;;

  let rec rename_expr (env : ident VM.t) ((loc, expr) : located_expr)
    : located_expr * ident VM.t
    =
    match expr with
    | Hole -> (loc, Hole), env
    | Const (s, Ident i) ->
      let i' = get_str i in
      (match VM.find_opt i' env with
       | Some i -> (loc, Const (s, Ident i)), env
       | None -> (loc, Const (s, Ident i)), env)
    | Const (s, AccessIdent is) ->
      let is =
        List.map
          (fun i ->
             match VM.find_opt (get_str i) env with
             | Some i -> i
             | None -> i)
          is
      in
      (loc, Const (s, AccessIdent is)), env
    | Const _ -> (loc, expr), env
    | Bop (l, op, r) ->
      let l, env = rename_expr env l in
      let r, env = rename_expr env r in
      (match op with
       | User_op i ->
         (match VM.find_opt (get_str i) env with
          | Some i -> (loc, Bop (l, User_op i, r)), env
          | None -> (loc, Bop (l, op, r)), env)
       | _ -> (loc, Bop (l, op, r)), env)
    | Ap (_, l, r) ->
      let l, env = rename_expr env l in
      let r, env = rename_expr env r in
      let i = find_ident l in
      (match Base.List.Assoc.find builtins ~equal:Base.String.( = ) i with
       | Some b -> (loc, Ap (b, l, r)), env
       | None -> (loc, Ap (fresh_binder (), l, r)), env)
    | Tuple es ->
      let es, env = rename_list ~f:rename_expr env es in
      (loc, Tuple es), env
    | Let (p, t, expr, n) ->
      let rec collect_idents acc = function
        | _, PConst (_, Ident i) -> i :: acc
        | _, PConst (_, AccessIdent is) -> is @ acc
        | _, PBop (l, _, r) -> collect_idents acc l |> Fun.flip collect_idents r
        | _, PCtor (_, ps) ->
          let rec go a = function
            | [] -> a
            | h :: t ->
              let a' = collect_idents a h in
              go a' t
          in
          acc @ go [] ps
        | _ -> acc
      in
      (*TODO: shadow warning? *)
      let p, env = rename_pattern env p in
      let t =
        match t with
        | Some t -> Some (fst @@ rename_expr env t)
        | None -> None
      in
      let expr, _ = rename_expr env expr in
      let env =
        let idents =
          collect_idents [] p
          |> List.map (fun i ->
            let i = get_str i in
            let i' = fresh_alpha i in
            i, i')
        in
        let env = VM.to_list env in
        idents @ env |> VM.of_list
      in
      let n, env = rename_expr env n in
      (loc, Let (p, t, expr, n)), env
    | Lam (p, t) ->
      let p, env = rename_pattern env p in
      let t, _ = rename_expr env t in
      (loc, Lam (p, t)), env
    | Match (e, bs) ->
      let e, _ = rename_expr env e in
      let bs =
        List.map
          (fun (p, wb, expr) ->
             let p, env = rename_pattern env p in
             let wb = Base.Option.map wb ~f:(Fun.compose fst (rename_expr env)) in
             let expr, _ = rename_expr env expr in
             p, wb, expr)
          bs
      in
      (loc, Match (e, bs)), env
    | TypeLit p -> (loc, TypeLit p), env
    | Binding (i, e, is_imp) ->
      let i = get_str i in
      let i' = fresh_alpha i in
      let env = VM.add i i' env in
      (loc, Binding (i', e, is_imp)), env
    | Pi (l, r) ->
      let l, env = rename_expr env l in
      let r, env = rename_expr env r in
      (loc, Pi (l, r)), env
  ;;

  let rec rename_definition
            ((defenv, varenv) : ident VM.t * ident VM.t)
            ((loc, d) : located_definition)
    : located_definition * (ident VM.t * ident VM.t)
    =
    let combine e1 e2 = VM.of_list @@ VM.to_list e1 @ VM.to_list e2 in
    match d with
    | Dec (i, sig') ->
      let i, defenv =
        if get_str i = "main"
        then i, defenv
        else (
          let i = get_str i in
          match VM.find_opt i defenv with
          | Some i' -> i', defenv
          | _ ->
            let i' = fresh_alpha i in
            let defenv = VM.add i i' defenv in
            i', defenv)
      in
      let sig', _ = rename_expr (combine defenv varenv) sig' in
      (loc, Dec (i, sig')), (defenv, varenv)
    | Def (i, when_block, body, with_block) ->
      let i, defenv =
        if get_str i = "main"
        then i, defenv
        else (
          let i = get_str i in
          match VM.find_opt i defenv with
          | Some i -> i, defenv
          | _ ->
            let i' = fresh_alpha i in
            let defenv = VM.add i i' defenv in
            i', defenv)
      in
      let env = combine defenv varenv in
      let when_block, env =
        match when_block with
        | Some wb ->
          let t, e = rename_expr env wb in
          Some t, e
        | None -> None, env
      in
      let with_block, env =
        rename_list
          ~f:(fun env d ->
            let def, (env, _) = rename_definition (env, fresh_env ()) d in
            def, env)
          env
          with_block
      in
      let body, _ = rename_expr env body in
      (loc, Def (i, when_block, body, with_block)), (defenv, varenv)
  ;;

  let rename_program ((prog_name, imps, tys, defs) : program) : program =
    let defs =
      rename_list
        ~f:(fun env d ->
          let def, (env, _) = rename_definition (env, fresh_env ()) d in
          def, env)
        (fresh_env ())
        defs
      |> fst
    in
    prog_name, imps, tys, defs
  ;;
end
