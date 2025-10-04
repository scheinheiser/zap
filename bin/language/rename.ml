open Util

module Alpha : sig
  val user_bind : int
  val find_ident : Ast.located_expr -> string
  val rename_program : Ast.program -> Ast.program
end = struct
  module VM = Map.Make (String)

  (* any binder above this value is user defined *)
  let user_bind = 1
  let builtins = [ "print", 0; "::", 1 ]

  let fresh_binder =
    let i = ref 1 in
    fun () ->
      incr i;
      !i
  ;;

  let fresh_alpha =
    let i = ref (-1) in
    fun s ->
      incr i;
      s ^ string_of_int !i
  ;;

  let fresh_env () = VM.empty

  let rename_list
        ~(f : string VM.t -> 'a -> 'a * string VM.t)
        (env : string VM.t)
        (l : 'a list)
    : 'a list * string VM.t
    =
    let rec go acc e = function
      | [] -> List.rev acc, e
      | h :: t ->
        let h', e' = f e h in
        go (h' :: acc) e' t
    in
    go [] env l
  ;;

  let rec find_ident ((_, e) : Ast.located_expr) : string =
    let open Ast in
    match e with
    | Const (_, Ident i) -> i
    | Ap (_, l, _) -> find_ident l
    | _ -> ""
  ;;

  (* | b -> Error.report_err (Some loc, Format.asprintf "Expected function identifier, but got %a." Ast.pp_expr (loc, b)) *)

  let rec rename_pattern (env : string VM.t) ((loc, pat) : Ast.located_pattern)
    : Ast.located_pattern * string VM.t
    =
    let open Ast in
    match pat with
    | PConst (s, Ident i) ->
      let i' = fresh_alpha i in
      let env' = VM.add i i' env in
      (loc, PConst (s, Ident i')), env'
    | PConst _ -> (loc, pat), env
    | PWild -> (loc, pat), env
    | PCons (l, r) ->
      let l', env' = rename_pattern env l in
      let r', env'' = rename_pattern env' r in
      (loc, PCons (l', r')), env''
    | PList items ->
      let items', env' = rename_list ~f:rename_pattern env items in
      (loc, PList items'), env'
    | PTup items ->
      let items', env' = rename_list ~f:rename_pattern env items in
      (loc, PTup items'), env'
  ;;

  let rec rename_expr (env : string VM.t) ((loc, expr) : Ast.located_expr)
    : Ast.located_expr * string VM.t
    =
    let open Ast in
    match expr with
    | Const (s, Ident i) ->
      (match VM.find_opt i env with
       | Some i' -> (loc, Const (s, Ident i')), env
       | None -> (loc, Const (s, Ident i)), env)
    | Const _ -> (loc, expr), env
    | EList items ->
      let items', env' = rename_list ~f:rename_expr env items in
      (loc, EList items'), env'
    | ETup items ->
      let items', env' = rename_list ~f:rename_expr env items in
      (loc, ETup items'), env'
    | Bop (l, op, r) ->
      let l', env' = rename_expr env l in
      let r', env'' = rename_expr env' r in
      (match op with
       | User_op i ->
         (match VM.find_opt i env with
          | Some i' -> (loc, Bop (l', User_op i', r')), env
          | None -> (loc, Bop (l', op, r')), env)
       | _ -> (loc, Bop (l', op, r')), env'')
    | Ap (_, l, r) ->
      let l', env'' = rename_expr env l in
      let r', env' = rename_expr env r in
      let i = find_ident l' in
      (match Base.List.Assoc.find builtins ~equal:Base.String.( = ) i with
       | Some b -> (loc, Ap (b, l', r')), env'
       | None -> (loc, Ap (fresh_binder (), l', r')), env'')
  ;;

  let rec rename_term (env : string VM.t) ((loc, term) : Ast.located_term)
    : Ast.located_term * string VM.t
    =
    let open Ast in
    match term with
    | TExpr e ->
      let e, env = rename_expr env e in
      (loc, TExpr e), env
    | TLet (i, t, expr) ->
      let i' = fresh_alpha i in
      if VM.exists (fun i'' _ -> i'' = i) env
      then Error.report_warning (Some loc, Printf.sprintf "Identifier '%s' is shadowed." i);
      let expr', env' = rename_term env expr in
      let env'' = VM.add i i' env' in
      (loc, TLet (i', t, expr')), env''
    | TGrouping g ->
      let g', env' = rename_list ~f:rename_term env g in
      (loc, TGrouping g'), env'
    | TIf (e, t, f) ->
      let e', env' = rename_expr env e in
      let t', env'' = rename_term env' t in
      let f', env''' =
        match f with
        | Some f' ->
          let t, e' = rename_term env'' f' in
          Some t, e'
        | None -> None, env''
      in
      (loc, TIf (e', t', f')), env'''
    | TLam (ps, t) ->
      let ps', env' = rename_list ~f:rename_pattern env ps in
      let t', env'' = rename_term env' t in
      (loc, TLam (ps', t')), env''
  ;;

  let rec rename_definition (env : string VM.t) ((loc, d) : Ast.located_definition)
    : Ast.located_definition * string VM.t
    =
    let open Ast in
    match d with
    | Dec (i, sig') ->
      let i', env' =
        if i = "main"
        then i, env
        else (
          let i' = fresh_alpha i in
          let env' = VM.add i i' env in
          i', env')
      in
      (loc, Dec (i', sig')), env'
    | Def (i, args, when_block, body, with_block) ->
      let i', env =
        if i = "main"
        then i, env
        else (
          match VM.find_opt i env with
          | Some i' -> i', env
          | None ->
            let i' = fresh_alpha i in
            let env' = VM.add i i' env in
            i', env')
      in
      let args', env = rename_list ~f:rename_pattern env args in
      (match when_block with
       | Some wb ->
         let t, e' = rename_term env wb in
         Some t, e'
       | None -> None, env)
      |> fun (when_block', env) ->
      (match with_block with
       | Some wb ->
         let ds, e' = rename_list ~f:rename_definition env wb in
         Some ds, e'
       | None -> None, env)
      |> fun (with_block', env) ->
      let body', env = rename_list ~f:rename_term env body in
      (loc, Def (i', args', when_block', body', with_block')), env
  ;;

  let rename_program ((prog_name, imps, tys, defs) : Ast.program) : Ast.program =
    let defs' = fst @@ rename_list ~f:rename_definition (fresh_env ()) defs in
    prog_name, imps, tys, defs'
  ;;
end
