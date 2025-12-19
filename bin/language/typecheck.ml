open Util
module C = Map.Make (String)

let ( let* ) = Base.Or_error.( >>= )
let ( let@ ) = Base.Or_error.( >>| )

(* type, value *)
type ctx = (Typed_ast.located_expr * Typed_ast.located_expr option) C.t

(* utils *)
let lookup_ty ctx : string -> Typed_ast.located_expr option =
  Fun.compose (Base.Option.map ~f:fst) (fun k -> C.find_opt k ctx)

let lookup_value ctx : string -> Typed_ast.located_expr option option =
  Fun.compose (Base.Option.map ~f:snd) (fun k -> C.find_opt k ctx)

let extend ctx ?v n t = C.add n (t, v) ctx

let make_err (e : Error.t) : 'a Base.Or_error.t =
  Base.Or_error.error_string @@ Error.format_err e

let get_level = function
  | Ast.PUni ix -> ix + 1
  | _ -> 0

let rec replace (subs : (Ast.ident * Typed_ast.typed_expr) list)
    ((ty, e): Typed_ast.typed_expr) : Typed_ast.typed_expr =
  let go ((loc, e) as te) =
    let open Typed_ast in
    match e with
    | Const (_, Ast.Ident i) -> (
      let i = Ast.get_str_combine i in
      match
        List.map (fun (i, t) -> (Ast.get_str_combine i, t)) subs
        |> List.assoc_opt i
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
        (p, cond, b)
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

let rec unify_pattern_type (ctx: ctx) ((_, p): Ast.located_pattern) (((_, t') as t, e): Typed_ast.typed_expr) : ctx = 
  match p, t' with
  | (Ast.PConst (_, Ast.Ident i)), _ -> extend ctx (Ast.get_str_combine i) t ~v:e

let rec normalise (ctx: ctx) ((t, e): Typed_ast.typed_expr): (Typed_ast.typed_expr * ctx) Base.Or_error.t =
  let open Typed_ast in
  let open Base.Or_error in
  let rec go ctx (loc, e) = 
    match e with
    | Const (_, Ident i) ->
      (match lookup_value ctx (Ast.get_str_combine i) with
      | None -> make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." (Ast.get_str i))
      | Some v ->
        (match v with
        | None -> Ok ((loc, e), ctx)
        | Some v -> go ctx v))
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
      (loc, Binding (i, t)), extend ctx (Ast.get_str_combine i) (snd t)
    | Let (i, e, n) ->
      let last_expr =
        let rec go = function
          | _, (_, Let (_, _, n)) -> go n
          | e -> e
        in go e
      in failwith "what now"
  in 
  let* t, ctx = go ctx t in 
  let@ e, ctx = go ctx e in
  (t, e), ctx

let get_const_type ((loc, c) : Ast.located_const): Typed_ast.located_expr =
  let open Typed_ast in
  let e = 
    match c with
    | Ast.Int _ -> TypeLit Ast.PInt
    | Ast.Float _ -> TypeLit Ast.PFloat
    | Ast.String _ -> TypeLit Ast.PString
    | Ast.Char _ -> TypeLit Ast.PChar
    | Ast.Bool _ -> TypeLit Ast.PBool
    | Ast.Atom _ -> TypeLit Ast.PAtom
    | Ast.Unit -> TypeLit Ast.PUnit
    | Ast.Ident _ | Ast.Udc _ -> raise (Error.InternalError "called `get_const_type` on identifier.")
  in loc, e

let rec infer (ctx : ctx) ((loc, e) : Ast.located_expr) :
    (Typed_ast.typed_expr * ctx) Base.Or_error.t =
  let open Ast in
  match e with
  | Const (l, Ident i) -> (
    let i' = Ast.get_str_combine i in
    match lookup_ty ctx i' with
    | None ->
      make_err
        (Some loc, Printf.sprintf "Undefined identifier '%s'." (Ast.get_str i))
    | Some t -> 
      let e = t, (loc, Typed_ast.Const (l, Ident i)) in
      Ok (e, ctx))
  | Const c -> 
    let e = get_const_type c, (loc, Typed_ast.Const c) in
    Ok (e, ctx)
  | TypeLit l -> 
    let e = (loc, Typed_ast.TypeLit (PUni (get_level l))), (loc, Typed_ast.TypeLit l) in
    Ok (e, ctx)
  | Binding (i, e) ->
    let@ e, ctx = infer ctx e in
    (* the binding expression is given the type of the bound type. *)
    let e' = fst e, (loc, Typed_ast.Binding (i, e)) in
    e', extend ctx (Ast.get_str_combine i) (snd e) (* the identifier itself is associated with the bound type though *)
  | Pi (l, r) ->
    let* (lt, _) as l, ctx' = infer ctx l in
    let* (rt, _) as r, _ = infer ctx' r in
    let@ u1, u2 =
      match lt, rt with
      | (_, Typed_ast.TypeLit l), (_, Typed_ast.TypeLit r) -> Ok (get_level l, get_level r)
    in
    let e = (loc, Typed_ast.TypeLit (PUni (Int.max u1 u2))), (loc, Typed_ast.Pi (l, r)) in
    e, ctx
