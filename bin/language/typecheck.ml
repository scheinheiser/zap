open Util
open Primitive
module C = Map.Make (String)

let ignore_m = Base.Or_error.ignore_m
let ( let* ) = Base.Or_error.( >>= )
let ( let@ ) = Base.Or_error.( >>| )
let flip = Fun.flip

(* type, value *)
type ctx = Typed_ast.typed_expr C.t

let empty () : ctx = C.empty

(* utils *)
let lookup ctx : string -> Typed_ast.typed_expr option = flip C.find_opt ctx

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
  | Unit -> PUnit
  | Ident _ | Udc _ | AccessIdent _ ->
    raise (Error.InternalError "called `get_const_type` on identifier.")
;;

(* β-reduction *)
let rec reduce (ctx : ctx) ((ty, e) : Typed_ast.typed_expr) : Typed_ast.typed_expr =
  let go ((loc, e) as te) =
    let open Typed_ast in
    match e with
    | Const (_, Ident i) ->
      let i = get_str_combine i in
      (match lookup ctx i with
       | None -> te
       | Some e -> snd e)
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
     | None -> make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." (get_str i))
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
    let@ _ = combine_errors bs in
    ctx
  | Typed_ast.PConst (_, c), Typed_ast.Const (_, c') ->
    let lc = get_const_type c in
    let rc = get_const_type c' in
    if lc = rc then Ok ctx else uni_err loc p_with_loc t
  | Typed_ast.PConst (_, c), Typed_ast.Ap (_, _, _) ->
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
        | _ when List.length cons <> List.length ps ->
          Error.todo "empty variants" (*TODO:something something empty variants *)
        | _ when i <> i' ->
          make_err
            (Some loc, Printf.sprintf "Expected '%s' constructor, but got '%s'." i i')
        | _ ->
          let@ ctxs =
            List.map2 (fun p c -> unify_pat ctx p c) ps cons
            |> Base.Or_error.combine_errors
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
    | Ap (b, l, r) ->
      let* l, _ = normalise ctx l in
      let* r, _ = normalise ctx r in
      (match l with
       (* we check if it's a lambda to attempt β-reduction. *)
       | _, (_, Lam (p, b)) ->
         let@ ctx' = unify_pat ctx p r in
         snd @@ reduce ctx' b, ctx
       | _ -> Ok ((loc, Ap (b, l, r)), ctx))
    | Binding (i, t) ->
      let@ t, _ = normalise ctx t in
      (loc, Binding (i, t)), extend ctx (get_str_combine i) ~t:(fst t) ~v:(snd t)
    | Let (p, e, n) ->
      let* e, _ = normalise ctx e in
      let* ctx' = unify_pat ctx p e in
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
    | Tuple ts ->
      let@ ts =
        List.map (fun e -> let@ e, _ = normalise ctx e in e) ts 
        |> Base.Or_error.combine_errors
      in
      (loc, Tuple ts), ctx
    | _ -> Ok ((loc, e), ctx)
  in
  let* t, ctx = go ctx t in
  let@ e, ctx = go ctx e in
  (t, e), ctx
;;

(* checking that a given value is actually a type or is just an expression *)
let is_type (ctx : ctx) ((loc, e) : Typed_ast.located_expr) : bool Base.Or_error.t =
  let open Typed_ast in
  let lookup_id ctx i =
    match lookup_ty ctx i with
    | None -> make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." i)
    | Some _ -> Ok true
  in
  (* this is used to check the type of a type, hence why other typelits are ignored *)
  let is_uni (_, t) =
    match t with
    | TypeLit (PUni _) -> true
    | _ -> false
  in
  match e with
  | Const (_, Ident i) | Const (_, Udc i) -> lookup_id ctx (get_str_combine i)
  | Const (_, AccessIdent is) ->
    let i = List.rev is |> List.hd |> get_str_combine in
    lookup_id ctx i
  | Ap (_, (l, _), (r, _)) | Pi ((l, _), (r, _)) -> Ok (is_uni l && is_uni r)
  | Binding (_, (t, _)) | Lam (_, (t, _)) -> Ok (is_uni t)
  | Match (_, ts) -> Ok (List.fold_left (fun acc (_, _, (t, _)) -> acc && is_uni t) true ts)
  | Tuple ts -> Ok (List.fold_left (fun acc (t, _) -> acc && is_uni t) true ts)
  | Let (_, (te, _), (tn, _)) -> Ok (is_uni te && is_uni tn)
  | TypeLit _ -> Ok true
  | Const _ -> Ok false

(* type inference for expressions *)
let rec infer (ctx : ctx) ((loc, e) : Desugar.located_expr)
  : (Typed_ast.typed_expr * ctx) Base.Or_error.t
  =
  let open Desugar in
  match e with
  | Const (l, Ident i) ->
    let i' = get_str_combine i in
    (match lookup_ty ctx i' with
     | None -> make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." (get_str i))
     | Some t ->
       let e = t, (loc, Typed_ast.Const (l, Ident i)) in
       Ok (e, ctx))
  | Const (l, Udc i) ->
    let i' = get_str_combine i in
    (match lookup_ty ctx i' with
     | None -> make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." (get_str i))
     | Some t ->
       let e = t, (loc, Typed_ast.Const (l, Udc i)) in
       Ok (e, ctx))
  | Const (l, AccessIdent is) ->
    let i = List.map get_str is |> String.concat "." in
    let i' = List.rev is |> List.hd |> get_str_combine in
    (match lookup_ty ctx i' with
     | None -> make_err (Some loc, Printf.sprintf "Undefined identifier '%s'." i)
     | Some t ->
       let e = t, (loc, Typed_ast.Const (l, AccessIdent is)) in
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
  | Binding (i, e, _) ->
    let* (_, fail) as e, ctx = infer ctx e in
    let* ty = is_type ctx (snd e) in
    if not ty
    then make_err (Some loc, Format.asprintf "Expected type, but got %a." Typed_ast.pp_expr fail)
    else
      (* the 'binding' expression is given the type of the bound type. *)
      let e' = fst e, (loc, Typed_ast.Binding (i, e)) in
      (* the identifier itself is associated with the bound type *)
      Ok (e', extend ctx (get_str_combine i) ~t:(snd e) ~v:(snd e'))
  | Pi (l, r) ->
    let* l, ctx' = infer ctx l in
    let* r, _ = infer ctx' r in
    let* u1 = infer_universe ctx' l in
    let@ u2 = infer_universe ctx' r in
    let e = (loc, Typed_ast.TypeLit (PUni (Int.max u1 u2))), (loc, Typed_ast.Pi (l, r)) in
    e, ctx
  | Ap (b, l, r) ->
    let* l, ctx' = infer ctx l in
    let* r, _ = infer ctx' r in
    let* (lt, _) as l, _ = normalise ctx' l in
    let* (rt, _) as r, _ = normalise ctx' r in
    (match lt with
    | _, Typed_ast.Pi ((_, lt), (_, ret)) ->
      let@ _ = check_equal ctx ~e:rt ~g:lt in
      let e = loc, Typed_ast.Ap (b, l, r) in
      (ret, e), ctx
    | _ ->
      make_err (Some loc, Format.asprintf "Expected a function, but got %a." Typed_ast.pp_typed_expr l))
  | _ -> Error.todo "finish infer"

and infer_universe (ctx : ctx) (t : Typed_ast.typed_expr) : int Base.Or_error.t =
  let open Typed_ast in
  let* ((_, t), ((loc, _) as e)), _ = normalise ctx t in
  match t with
  | TypeLit t -> Ok (get_level t)
  | _ -> make_err (Some loc, Format.asprintf "Expected type, got '%a'." pp_expr e)

and check_equal (ctx : ctx) ~e:(l : Typed_ast.located_expr) ~g:(r : Typed_ast.located_expr) : unit Base.Or_error.t =
  let open Typed_ast in
  let (_, l) as l' = to_nf ctx l in
  let (loc, r) as r' = to_nf ctx r in
  let* lres = is_type ctx l' in
  let* rres = is_type ctx r' in
  match lres, rres with
  | false, false -> make_err (Some loc, Format.asprintf "Expected types, got %a and %a." pp_expr l' pp_expr r')
  | false, _ -> make_err (Some loc, Format.asprintf "Expected type, got %a." pp_expr l')
  | _, false -> make_err (Some loc, Format.asprintf "Expected type, got %a." pp_expr r')
  | _ ->
    let eq_err ~ex ~got =
      make_err (Some loc, Printf.sprintf "Expected type %s, but got %s." ex got)
    in
    match l, r with
    | Const (_, Ident l), Const (_, Ident r) | Const (_, Udc l), Const (_, Udc r) -> 
      let l = get_str_combine l in
      let r = get_str_combine r in
      if l = r
      then Ok ()
      else eq_err ~ex:l ~got:r
    | TypeLit l, TypeLit r ->
      (match l, r with
      | PInt, PInt
      | PFloat, PFloat
      | PString, PString
      | PChar, PChar
      | PBool, PBool
      | PUnit, PUnit -> Ok ()
      | PUni l, PUni r when l = r -> Ok ()
      | _ -> eq_err ~ex:(show_prim l) ~got:(show_prim r))
    | Ap (_, (_, ll), (_, lr)), Ap (_, (_, rl), (_, rr)) ->
      let* _ = check_equal ctx ~e:ll ~g:rl in
      ignore_m @@ check_equal ctx ~e:lr ~g:rr
    | Tuple ls, Tuple rs when List.length ls = List.length rs ->
      let ls = List.map fst ls in
      let rs = List.map fst rs in
      List.map2 (fun l r -> check_equal ctx ~e:l ~g:r) ls rs
      |> Base.Or_error.combine_errors_unit
    | Binding (li, (_, l)), Binding (ri, (_, r)) when (get_str_combine li) = (get_str_combine ri) -> check_equal ctx ~e:l ~g:r
    | Pi ((_, ll), (_, lr)), Pi ((_, rl), (_, rr)) ->
      let* _ = check_equal ctx ~e:ll ~g:rl in
      check_equal ctx ~e:lr ~g:rr
    | Let (_, (_, lb), (_, ln)), Let (_, (_, rb), (_, rn)) ->
      let* _ = check_equal ctx ~e:lb ~g:rb in
      ignore_m (check_equal ctx ~e:ln ~g:rn)
    | Match ((_, lc), lbs), Match ((_, rc), rbs) when List.length lbs = List.length rbs ->
      let* _ = check_equal ctx ~e:lc ~g:rc in
      List.map2 (fun (_, _, (_, l)) (_, _, (_, r)) -> check_equal ctx ~e:l ~g:r) lbs rbs |> Base.Or_error.combine_errors_unit
    | _ -> raise (Error.InternalError "Internal error - non-type value.")

and to_nf (_ : ctx) (_ : Typed_ast.located_expr) : Typed_ast.located_expr =
  Error.todo "to_nf"
;;
