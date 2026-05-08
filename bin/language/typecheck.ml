open Util
module C = Map.Make (String)

let ( let* ) = Base.Or_error.( >>= )
let ( let@ ) = Base.Or_error.( >>| )

(* type, value *)
type ctx = Typed_ast.typed_expr C.t

let empty () : ctx = C.empty

(* utils *)
let lookup ctx : string -> Typed_ast.typed_expr option = Fun.flip C.find_opt ctx

let lookup_ty ctx : string -> Typed_ast.located_expr option =
  Fun.compose (Base.Option.map ~f:fst) (lookup ctx)
;;

let lookup_value ctx : string -> Typed_ast.located_expr option =
  Fun.compose (Base.Option.map ~f:snd) (lookup ctx)
;;

let extend ctx n ~t ~v = C.add n (t, v) ctx

let make_err (e : Error.t) : 'a Base.Or_error.t =
  Base.Or_error.error_string @@ Error.format_err e
;;

let get_level = function
  | Ast.PUni ix -> ix + 1
  | _ -> 0
;;

let get_const_type (c : Ast.const) : Ast.prim =
  match c with
  | Ast.Int _ -> Ast.PInt
  | Ast.Float _ -> Ast.PFloat
  | Ast.String _ -> Ast.PString
  | Ast.Char _ -> Ast.PChar
  | Ast.Bool _ -> Ast.PBool
  | Ast.Atom _ -> Ast.PAtom
  | Ast.Ident _ | Ast.Udc _ ->
    raise (Error.InternalError "called `get_const_type` on identifier.")
;;

let rec replace
          (subs : (Ast.ident * Typed_ast.typed_expr) list)
          ((ty, e) : Typed_ast.typed_expr)
  : Typed_ast.typed_expr
  =
  let go ((loc, e) as te) =
    let open Typed_ast in
    match e with
    | Const (_, Ast.Ident i) ->
      let i = Ast.get_str_combine i in
      (match
         List.map (fun (i, t) -> Ast.get_str_combine i, t) subs |> List.assoc_opt i
       with
       | None -> te
       | Some e -> snd e)
    | List es ->
      let es = List.map (fun e -> replace subs e) es in
      loc, List es
    | Bop (l, op, r) ->
      let l = replace subs l
      and r = replace subs r in
      loc, Bop (l, op, r)
    | Ap (b, l, r) ->
      let l = replace subs l
      and r = replace subs r in
      loc, Ap (b, l, r)
    | Let (p, e, next) ->
      let e = replace subs e
      and next = replace subs next in
      loc, Let (p, e, next)
    | If (c, tr, f) ->
      let c = replace subs c
      and tr = replace subs tr
      and f = Base.Option.map ~f:(fun f -> replace subs f) f in
      loc, If (c, tr, f)
    | Lam (ps, b) ->
      let b = replace subs b in
      loc, Lam (ps, b)
    | Match (c, bs) ->
      let sub_branch (p, cond, b) =
        let cond = Base.Option.map ~f:(fun c -> replace subs c) cond
        and b = replace subs b in
        p, cond, b
      in
      let c = replace subs c
      and bs = List.map sub_branch bs in
      loc, Match (c, bs)
    | Binding (i, e) ->
      let e = replace subs e in
      loc, Binding (i, e)
    | Pi (l, r) ->
      let l = replace subs l
      and r = replace subs r in
      loc, Pi (l, r)
    | e -> loc, e
  in
  go ty, go e
;;

(*NOTE: 
  there may be an issue with how match/if cases are handled.
  it means that the identifier (if there is one) takes on the expression of the false branch or the last match branch, which could affect normalisation...
*)
let rec unify
          (ctx : ctx)
          ((_, p) as p_with_loc : Ast.located_pattern)
          ((((_, t') as t), ((loc, _) as e)) : Typed_ast.typed_expr)
  : ctx Base.Or_error.t
  =
  let uni_err loc pattern t =
    let msg =
      Format.asprintf
        "Failed to unify pattern '%s' with type %a."
        (Ast.show_pat pattern)
        Typed_ast.pp_expr
        t
    in
    make_err (Some loc, msg)
  in
  match p, t' with
  | _, Typed_ast.Const (_, Ast.Ident i) ->
    (match lookup ctx (Ast.get_str_combine i) with
     | None ->
       make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." (Ast.get_str i))
     | Some e -> unify ctx p_with_loc e)
  | Ast.PConst (_, Ast.Ident i), _ -> Ok (extend ctx (Ast.get_str_combine i) ~t ~v:e)
  | _, Typed_ast.If (_, tr, fa) ->
    let* ctx = unify ctx p_with_loc tr in
    (match fa with
     | None -> Ok ctx
     | Some fa -> unify ctx p_with_loc fa)
  | _, Typed_ast.Match (_, bs) ->
    let open Base.Or_error in
    let rec go ctx acc = function
      | [] -> List.rev acc, ctx
      | (_, _, e) :: t ->
        (match unify ctx p_with_loc e with
         | Error _ as err -> go ctx (err :: acc) t
         | Ok ctx -> go ctx (Ok () :: acc) t)
    in
    let bs, ctx = go ctx [] bs in
    combine_errors bs >>| fun _ -> ctx
  | Ast.PCons (l, r), Typed_ast.List (e :: es) ->
    let* ctx = unify ctx l e in
    let es = t, (loc, Typed_ast.List es) in
    unify ctx r es
  | Ast.PConst (_, c), Typed_ast.Const (_, c') ->
    let lc = get_const_type c in
    let rc = get_const_type c' in
    if lc = rc then Ok ctx else uni_err loc p_with_loc t
  | Ast.PConst (_, c), Typed_ast.Bop (_, _, _) | Ast.PConst (_, c), Typed_ast.Ap (_, _, _)
    ->
    let _ = get_const_type c in
    failwith "finish normalise! for equality function"
  | Ast.PCtor (i, ps), Typed_ast.Ap (_, l, r) ->
    let rec find_ident (_, e) =
      let open Typed_ast in
      match e with
      | _, Const (_, Ident i) -> Ast.get_str_combine i, false
      | _, Const (_, Udc i) -> Ast.get_str_combine i, true
      | _, Ap (_, l, _) -> find_ident l
      | _ -> "", false
    in
    (match find_ident l with
     | "", _ | _, false ->
       make_err
         ( Some loc
         , Format.asprintf
             "Expected a constructor, but got '%a'."
             Typed_ast.pp_typed_expr
             l )
     | i', _ ->
       let i = Ast.get_str_combine i in
       let cons =
         let rec go acc = function
           | _, (_, Typed_ast.Ap (_, l, r)) -> go (r :: acc) l
           | _ -> acc
         in
         List.rev @@ (r :: go [] l)
       in
       (match () with
        | _ when List.length cons <> List.length ps -> failwith ""
        | _ when i <> i' ->
          make_err
            (Some loc, Printf.sprintf "Expected '%s' constructor, but got '%s'." i i')
        | _ ->
          let@ ctxs =
            List.map2 (fun p c -> unify ctx p c) ps cons |> Base.Or_error.combine_errors
          in
          List.rev ctxs |> List.hd))
  | _ -> uni_err loc p_with_loc t

and normalise (ctx : ctx) ((t, e) : Typed_ast.typed_expr)
  : (Typed_ast.typed_expr * ctx) Base.Or_error.t
  =
  let open Typed_ast in
  let open Base.Or_error in
  let rec go ctx (loc, e) =
    match e with
    | Const (_, Ident i) ->
      (match lookup_value ctx (Ast.get_str_combine i) with
       | None ->
         make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." (Ast.get_str i))
       | Some v -> go ctx v)
    | Const _ -> Ok ((loc, e), ctx)
    | List es ->
      let@ es = List.map (fun e -> normalise ctx e |> map ~f:fst) es |> combine_errors in
      (loc, List es), ctx
    | Bop (l, op, r) ->
      let* l, _ = normalise ctx l in
      let@ r, _ = normalise ctx r in
      (loc, Bop (l, op, r)), ctx
    | Ap (b, l, r) ->
      let* l, _ = normalise ctx l in
      let@ r, _ = normalise ctx r in
      (match l with
       (*TODO: handle lambda case*)
       | l -> (loc, Ap (b, l, r)), ctx)
    | Binding (i, t) ->
      let@ t, _ = normalise ctx t in
      (loc, Binding (i, t)), extend ctx (Ast.get_str_combine i) ~t:(fst t) ~v:(snd t)
    | Let (p, e, n) ->
      let last_expr =
        let rec go = function
          | _, (_, Let (_, _, n)) -> go n
          | e -> e
        in
        go e
      in
      let* e, _ = normalise ctx e in
      let* ctx' = unify ctx p last_expr in
      let@ n, _ = normalise ctx' n in
      (loc, Let (p, e, n)), ctx
    | If (c, tr, fa) ->
      let* c, _ = normalise ctx c in
      let* tr, _ = normalise ctx tr in
      (match fa with
       | None -> Ok ((loc, If (c, tr, fa)), ctx)
       | Some fa ->
         let@ fa, _ = normalise ctx fa in
         (loc, If (c, tr, Some fa)), ctx)
    | Match (c, bs) ->
      let* c, _ = normalise ctx c in
      let normalise_branch ctx (p, wb, b) =
        let* ctx = unify ctx p c in
        let* wb =
          match wb with
          | None -> Ok wb
          | Some wb ->
            let@ wb, _ = normalise ctx wb in
            Some wb
        in
        let@ b, _ = normalise ctx b in
        p, wb, b
      in
      let@ bs = List.map (normalise_branch ctx) bs |> Base.Or_error.combine_errors in
      (loc, Match (c, bs)), ctx
    | Lam (ps, b) ->
      (*NOTE: this may cause issues, keep an eye on it *)
      let@ b, _ = normalise ctx b in
      (loc, Lam (ps, b)), ctx
    | Pi (l, r) ->
      let* l, ctx' = normalise ctx l in
      let@ r, _ = normalise ctx' r in
      (loc, Pi (l, r)), ctx
    | _ -> Ok ((loc, e), ctx)
  in
  let* t, ctx = go ctx t in
  let@ e, ctx = go ctx e in
  (t, e), ctx
;;

let rec infer (ctx : ctx) ((loc, e) : Ast.located_expr)
  : (Typed_ast.typed_expr * ctx) Base.Or_error.t
  =
  let open Ast in
  match e with
  | Const (l, Ident i) ->
    let i' = Ast.get_str_combine i in
    (match lookup_ty ctx i' with
     | None ->
       make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." (Ast.get_str i))
     | Some t ->
       let e = t, (loc, Typed_ast.Const (l, Ident i)) in
       Ok (e, ctx))
  | Const (l, c) ->
    let e = (loc, Typed_ast.TypeLit (get_const_type c)), (loc, Typed_ast.Const (l, c)) in
    Ok (e, ctx)
  | TypeLit l ->
    let e = (loc, Typed_ast.TypeLit (PUni (get_level l))), (loc, Typed_ast.TypeLit l) in
    Ok (e, ctx)
  | Binding (i, e) ->
    let@ e, ctx = infer ctx e in
    (* the binding expression is given the type of the bound type. *)
    let e' = fst e, (loc, Typed_ast.Binding (i, e)) in
    e', extend ctx (Ast.get_str_combine i) ~t:(fst e') ~v:(snd e')
    (* the identifier itself is associated with the bound type *)
  | Pi (l, r) ->
    let* l, ctx' = infer ctx l in
    let* r, _ = infer ctx' r in
    (*TODO: factor this out to a function for more precise errors. *)
    let@ u1, u2 =
      let open Typed_ast in
      let* (_, (_, lt)), _ = normalise ctx l in
      let* (_, (_, rt)), _ = normalise ctx r in
      match lt, rt with
      | TypeLit l, TypeLit r -> Ok (get_level l, get_level r)
      | _ ->
        make_err
          (Some loc, Format.asprintf "Expected type, got '%a'." Ast.pp_expr (loc, e))
    in
    let e = (loc, Typed_ast.TypeLit (PUni (Int.max u1 u2))), (loc, Typed_ast.Pi (l, r)) in
    e, ctx
  | _ -> failwith ""
;;
