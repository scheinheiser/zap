open Util
open Primitive

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
  | PUni ix -> ix
  | _ -> 0
;;

let get_const_type (c : const) : prim =
  match c with
  | Int _ -> PInt
  | Float _ -> PFloat
  | String _ -> PString
  | Char _ -> PChar
  | Bool _ -> PBool
  | Atom _ -> PAtom
  | Unit -> PUnit
  | Ident _ | Udc _ ->
    raise (Error.InternalError "called `get_const_type` on identifier.")
;;

(* β-reduction *)
let rec reduce
          (ctx : ctx)
          ((ty, e) : Typed_ast.typed_expr)
  : Typed_ast.typed_expr
  =
  let go ((loc, e) as te) =
    let open Typed_ast in
    match e with
    | Const (_, Ident i) ->
      let i = get_str_combine i in
      (match lookup ctx i with
       | None -> te
       | Some e -> snd e)
    | Bop (l, op, r) ->
      let l = reduce ctx l
      and r = reduce ctx r in
      loc, Bop (l, op, r)
    | Ap (b, l, r) ->
      let l = reduce ctx l
      and r = reduce ctx r in
      loc, Ap (b, l, r)
    | Let (p, e, next) ->
      let e = reduce ctx e
      and next = reduce ctx next in
      loc, Let (p, e, next)
    | Lam (p, b) ->
      let b = reduce ctx b in
      loc, Lam (p, b)
    | Match (c, bs) ->
      let sub_branch (p, cond, b) =
        let cond = Base.Option.map ~f:(fun c -> reduce ctx c) cond
        and b = reduce ctx b in
        p, cond, b
      in
      let c = reduce ctx c
      and bs = List.map sub_branch bs in
      loc, Match (c, bs)
    | Binding (i, e) ->
      let e = reduce ctx e in
      loc, Binding (i, e)
    | Pi (l, r) ->
      let l = reduce ctx l
      and r = reduce ctx r in
      loc, Pi (l, r)
    | e -> loc, e
  in
  go ty, go e
;;

(* checks that a pattern and a given expression can be matched *)
let rec unify_pat
          (ctx : ctx)
          ((_, p) as p_with_loc : Typed_ast.located_pattern)
          ((((_, t') as t), ((loc, _) as e)) : Typed_ast.typed_expr)
  : ctx Base.Or_error.t
  =
  let uni_err loc pattern t =
    let msg =
      Format.asprintf
        "Failed to unify pattern '%s' with type %a."
        (Typed_ast.show_pat pattern)
        Typed_ast.pp_expr
        t
    in
    make_err (Some loc, msg)
  in
  match p, t' with
  | _, Typed_ast.Const (_, Ident i) ->
    (match lookup ctx (get_str_combine i) with
     | None ->
       make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." (get_str i))
     | Some e -> unify_pat ctx p_with_loc e)
  | Typed_ast.PConst (_, Ident i), _ -> Ok (extend ctx (get_str_combine i) ~t ~v:e)
  | _, Typed_ast.Match (_, bs) ->
    let open Base.Or_error in
    let rec go ctx acc = function
      | [] -> List.rev acc, ctx
      | (_, _, e) :: t ->
        (match unify_pat ctx p_with_loc e with
         | Error _ as err -> go ctx (err :: acc) t
         | Ok ctx -> go ctx (Ok () :: acc) t)
    in
    let bs, ctx = go ctx [] bs in
    combine_errors bs >>| fun _ -> ctx
  | Typed_ast.PCons (l, r), Typed_ast.Bop (l', Cons, r') ->
    let* ctx = unify_pat ctx l l' in
    unify_pat ctx r r'
  | Typed_ast.PConst (_, c), Typed_ast.Const (_, c') ->
    let lc = get_const_type c in
    let rc = get_const_type c' in
    if lc = rc then Ok ctx else uni_err loc p_with_loc t
  | Typed_ast.PConst (_, c), Typed_ast.Bop (_, _, _) | Typed_ast.PConst (_, c), Typed_ast.Ap (_, _, _)
    ->
    let _ = get_const_type c in
    failwith "finish normalise! for equality function"
  | Typed_ast.PCtor (i, ps), Typed_ast.Ap (_, l, r) ->
    let rec find_ident (_, e) =
      let open Typed_ast in
      match e with
      | _, Const (_, Ident i) -> get_str_combine i, false
      | _, Const (_, Udc i) -> get_str_combine i, true
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
       let i = get_str_combine i in
       let cons =
         let rec go acc = function
           | _, (_, Typed_ast.Ap (_, l, r)) -> go (r :: acc) l
           | _ -> acc
         in
         List.rev @@ (r :: go [] l)
       in
       (match () with
        | _ when List.length cons <> List.length ps -> failwith "" (*TODO:something something empty variants *)
        | _ when i <> i' ->
          make_err
            (Some loc, Printf.sprintf "Expected '%s' constructor, but got '%s'." i i')
        | _ ->
          let@ ctxs =
            List.map2 (fun p c -> unify_pat ctx p c) ps cons |> Base.Or_error.combine_errors
          in
          List.rev ctxs |> List.hd))
  | _ -> uni_err loc p_with_loc t

(* essentially evaluates expressions to (try and) get the underlying type *)
and normalise (ctx : ctx) ((t, e) : Typed_ast.typed_expr)
  : (Typed_ast.typed_expr * ctx) Base.Or_error.t
  =
  let open Typed_ast in
  let rec go ctx (loc, e) =
    match e with
    | Const (_, Ident i) ->
      (match lookup_value ctx (get_str_combine i) with
       | None ->
         make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." (get_str i))
       | Some v -> go ctx v)
    | Const _ -> Ok ((loc, e), ctx)
    | Bop (l, op, r) ->
      let* l, _ = normalise ctx l in
      let@ r, _ = normalise ctx r in
      (loc, Bop (l, op, r)), ctx
    | Ap (b, l, r) ->
      let* l, _ = normalise ctx l in
      let* r, _ = normalise ctx r in
      (match l with
       (* we check if it's a lambda to attempt β-reduction. *)
       | _, (_, Lam (p, b)) ->
          let@ ctx' = unify_pat ctx p r in
          (snd @@ reduce ctx' b), ctx
       | _ -> Ok ((loc, Ap (b, l, r)), ctx))
    | Binding (i, t) ->
      let@ t, _ = normalise ctx t in
      (loc, Binding (i, t)), extend ctx (get_str_combine i) ~t:(fst t) ~v:(snd t)
    | Let (p, e, n) ->
      let last_expr =
        let rec go = function
          | _, (_, Let (_, _, n)) -> go n
          | e -> e
        in
        go e
      in
      let* e, _ = normalise ctx e in
      let* ctx' = unify_pat ctx p last_expr in
      let@ n, _ = normalise ctx' n in
      (loc, Let (p, e, n)), ctx
    | Match (c, bs) ->
      let* c, _ = normalise ctx c in
      let normalise_branch ctx (p, wb, b) =
        let* ctx = unify_pat ctx p c in
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
    | Lam (p, b) ->
      let@ b, _ = normalise ctx b in
      (loc, Lam (p, b)), ctx
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

(* type inference for expressions *)
let rec infer (ctx : ctx) ((loc, e) : Desugar.located_expr)
  : (Typed_ast.typed_expr * ctx) Base.Or_error.t
  =
  let open Desugar in
  match e with
  | Const (l, Ident i) ->
    let i' = get_str_combine i in
    (match lookup_ty ctx i' with
     | None ->
       make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." (get_str i))
     | Some t ->
       let e = t, (loc, Typed_ast.Const (l, Ident i)) in
       Ok (e, ctx))
  | Const (l, c) ->
    let e = (loc, Typed_ast.TypeLit (get_const_type c)), (loc, Typed_ast.Const (l, c)) in
    Ok (e, ctx)
  | TypeLit l ->
    (* the type of Typeₖ is Typeₖ₊₁ *)
    let ix = 
      match l with
      | PUni ix -> ix + 1
      | _ -> 0
    in
    let e = (loc, Typed_ast.TypeLit (PUni ix)), (loc, Typed_ast.TypeLit l) in
    Ok (e, ctx)
  | Binding (i, e) ->
    let@ e, ctx = infer ctx e in
    (* the 'binding' expression is given the type of the bound type. *)
    let e' = fst e, (loc, Typed_ast.Binding (i, e)) in
    (* the identifier itself is associated with the bound type *)
    e', extend ctx (get_str_combine i) ~t:(snd e) ~v:(snd e')
  | Pi (l, r) ->
    let* l, ctx' = infer ctx l in
    let* r, _ = infer ctx' r in
    let* u1 = infer_universe ctx' l in
    let@ u2 = infer_universe ctx' r in
    let e = (loc, Typed_ast.TypeLit (PUni (Int.max u1 u2))), (loc, Typed_ast.Pi (l, r)) in
    e, ctx
  | _ -> failwith ""

and infer_universe (ctx: ctx) (t : Typed_ast.typed_expr) : int Base.Or_error.t =
  let open Typed_ast in
  let* ((_, t), ((loc, _) as e)), _ = normalise ctx t in
  match t with
  | TypeLit t -> Ok (get_level t)
  | _ -> 
    make_err
      (Some loc, Format.asprintf "Expected type, got '%a'." pp_expr e)
;;
