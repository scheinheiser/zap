open! Util

module Alpha : sig
  val user_bind : int
  val find_ident : Ast.located_expr -> string
  val fresh_alpha : string -> string
  val rename_program : Ast.program -> Ast.program
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
      s ^ string_of_int !i
  ;;

  let fresh_env () = VM.empty

  let rename_list ~(f : 'b VM.t -> 'a -> 'a * 'b VM.t) (env : 'b VM.t) (l : 'a list)
    : 'a list * 'b VM.t
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

  let rec rename_pattern (env : (string * bool) VM.t) ((loc, pat) : Ast.located_pattern)
    : Ast.located_pattern * (string * bool) VM.t
    =
    let open Ast in
    match pat with
    | PConst (s, Ident i) ->
      let i' = fresh_alpha i in
      let env = VM.add i (i', false) env in
      (loc, PConst (s, Ident i')), env
    | PConst _ -> (loc, pat), env
    | PWild -> (loc, pat), env
    | PCons (l, r) ->
      let l, env = rename_pattern env l in
      let r, env = rename_pattern env r in
      (loc, PCons (l, r)), env
    | PCtor (i, v) ->
      let i =
        match VM.find_opt i env with
        | Some (i', _) -> i'
        | None -> i
      in
      let v, env = rename_pattern env v in
      (loc, PCtor (i, v)), env
    | PList items ->
      let items, env = rename_list ~f:rename_pattern env items in
      (loc, PList items), env
  ;;

  let rec rename_expr (env : (string * bool) VM.t) ((loc, expr) : Ast.located_expr)
    : Ast.located_expr * (string * bool) VM.t
    =
    let open Ast in
    match expr with
    | Const (s, Ident i) ->
      (match VM.find_opt i env with
       | Some (i', _) -> (loc, Const (s, Ident i')), env
       | None -> (loc, Const (s, Ident i)), env)
    | Const _ -> (loc, expr), env
    | List items ->
      let items', env' = rename_list ~f:rename_expr env items in
      (loc, List items'), env'
    | Bop (l, op, r) ->
      let l', env' = rename_expr env l in
      let r', env'' = rename_expr env' r in
      (match op with
       | User_op i ->
         (match VM.find_opt i env with
          | Some (i', _) -> (loc, Bop (l', User_op i', r')), env
          | None -> (loc, Bop (l', op, r')), env)
       | _ -> (loc, Bop (l', op, r')), env'')
    | Ap (_, l, r) ->
      let l', env'' = rename_expr env l in
      let r', env' = rename_expr env r in
      let i = find_ident l' in
      (match Base.List.Assoc.find builtins ~equal:Base.String.( = ) i with
       | Some b -> (loc, Ap (b, l', r')), env'
       | None -> (loc, Ap (fresh_binder (), l', r')), env'')
    | Let (p, t, expr, n) ->
      let rec collect_idents acc = function
        | _, PConst (_, Ident i) -> i :: acc
        | _, PCons (l, r) -> collect_idents acc l |> Fun.flip collect_idents r
        | _, PList ps ->
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
            let i' = fresh_alpha i in
            i, (i', false))
        in
        let env = VM.to_list env in
        idents @ env |> VM.of_list
      in
      let n, env = rename_expr env n in
      (loc, Let (p, t, expr, n)), env
    | If (e, t, f) ->
      let e, env = rename_expr env e in
      let t, env = rename_expr env t in
      let f, env =
        match f with
        | Some f' ->
          let t, e = rename_expr env f' in
          Some t, e
        | None -> None, env
      in
      (loc, If (e, t, f)), env
    | Lam (ps, t) ->
      let ps, env = rename_list ~f:rename_pattern env ps in
      let t, _ = rename_expr env t in
      (loc, Lam (ps, t)), env
    | Match (e, bs) ->
      let e, _ = rename_expr env e in
      let bs =
        List.map
          (fun (p, wb, expr) ->
             let p, _ = rename_pattern env p in
             let wb = Base.Option.map wb ~f:(Fun.compose fst (rename_expr env)) in
             let expr, _ = rename_expr env expr in
             p, wb, expr)
          bs
      in
      (loc, Match (e, bs)), env
    | TypeLit p -> (loc, TypeLit p), env
    | Binding (i, e) -> (loc, Binding (i, e)), env (*TODO: cover this case *)
    | Pi (l, r) ->
      let l, env' = rename_expr env l in
      let r, _ = rename_expr env' r in
      (loc, Pi (l, r)), env
  ;;

  let rec rename_definition
            (env : (string * bool) VM.t)
            ((loc, d) : Ast.located_definition)
    : Ast.located_definition * (string * bool) VM.t
    =
    let open Ast in
    match d with
    | Dec (hsd, i, sig') ->
      let i, env =
        if i = "main"
        then i, env
        else (
          let i' = fresh_alpha i in
          let env' = VM.add i (i', hsd) env in
          i', env')
      in
      let sig', _ = rename_expr env sig' in
      (loc, Dec (hsd, i, sig')), env
    | Def (hsd, i, args, when_block, body, with_block) ->
      let i, env =
        if i = "main"
        then i, env
        else (
          match VM.find_opt i env with
          | Some (i', true) when hsd -> i', env
          | Some (i', false) when not hsd -> i', env
          | _ ->
            let i' = fresh_alpha i in
            let env' = VM.add i (i', hsd) env in
            i', env')
      in
      let args, env = rename_list ~f:rename_pattern env args in
      (match when_block with
       | Some wb ->
         let t, e = rename_expr env wb in
         Some t, e
       | None -> None, env)
      |> fun (when_block, env) ->
      rename_list ~f:rename_definition env with_block
      |> fun (with_block, env) ->
      let body, env = rename_expr env body in
      (loc, Def (hsd, i, args, when_block, body, with_block)), env
  ;;

  let rename_program ((prog_name, imps, tys, defs) : Ast.program) : Ast.program =
    let defs = rename_list ~f:rename_definition (fresh_env ()) defs |> fst in
    prog_name, imps, tys, defs
  ;;
end
