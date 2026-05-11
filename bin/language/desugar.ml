open Util
open Primitive

(* desugared syntax *)
type located_pattern = Location.t * pattern

and pattern =
  | PWild (* _ *)
  | PConst of located_const
  | PCons of located_pattern * located_pattern
  | PCtor of ident * located_pattern list
  | PTuple of located_pattern list

type located_expr = Location.t * expr

and expr =
  | Bop of located_expr * binop * located_expr
  | Ap of binder * located_expr * located_expr
  (* we give each function a binder to distinguish between user-defined functions and builtins later on *)
  | Tuple of located_expr list
  | Let of
      located_pattern
      * located_expr option
      * located_expr
      * located_expr (* let p₁ ... pₙ : <optional_ty> = e₁ in e₂ *)
  | Match of located_expr * (located_pattern * located_expr option * located_expr) list
  | Lam of located_pattern * located_expr
    (* `fun x y => x` is desugared to `fun x => fun y => x` *)
  | Const of located_const
  | TypeLit of prim
  | Binding of ident * located_expr (* x : T *)
  | Pi of located_expr * located_expr

type located_ty_decl = Location.t * ty_decl
and ty_decl = ident * tdecl_type

and tdecl_type =
  | Alias of located_expr
  | Variant of (ident * located_expr) list
  | Record of (ident * located_expr) list

type located_definition = Location.t * definition

and definition =
  | Dec of ident * located_expr
  | Def of ident * located_pattern list * located_expr option * located_expr * with_block
(* identifer, args, optional when-block, body, optional with-block *)

and with_block = located_definition list

type program =
  ident * located_import list * located_ty_decl list * located_definition list

(* this makes it easier to check if patterns match, making pattern matching on function arguments desugarable *)
type pattern_style =
  | SWild
  | SConst
  | SCons of pattern_style * pattern_style
  | SCtor of ident * pattern_style list
  | STuple of pattern_style list

let rec pattern_to_style ((_, p) : located_pattern): pattern_style =
  match p with
  | PWild -> SWild
  | PConst _ -> SConst
  | PCons (l, r) -> SCons (pattern_to_style l, pattern_to_style r)
  | PCtor (i, ps) -> SCtor (i, List.map pattern_to_style ps)
  | PTuple t -> STuple (List.map pattern_to_style t)

let rec equal_style (l : pattern_style) (r : pattern_style) : bool =
  match l, r with
  | SWild, SWild -> true
  | SConst, SConst -> true
  | SCons (ll, lr), SCons (rl, rr) -> equal_style ll lr && equal_style rl rr
  | SCtor (li, lps), SCtor (ri, rps) when (List.length lps = List.length rps) && (get_str_combine li = get_str_combine ri) -> 
    List.map2 equal_style lps rps |>
    List.fold_left (fun acc n -> acc && n) true
  | STuple l, STuple r when List.length l = List.length r ->
    List.map2 equal_style l r |>
    List.fold_left ( && ) true
  | _ -> false
and ( $= ) l r = equal_style l r

(* desugaring *)
let fresh_pattern_ident =
  let i = ref (-1) in
  fun () ->
    incr i;
    GStr ("__match__", !i)

let rec desugar_pat ((loc, e) : Ast.located_pattern): located_pattern = 
  match e with
  | Ast.PWild -> loc, PWild
  | Ast.PConst c -> loc, PConst c
  | Ast.PCons (l, r) ->
    let l = desugar_pat l in
    let r = desugar_pat r in
    loc, PCons (l, r)
  | Ast.PCtor (i, ps) ->
    let ps = List.map desugar_pat ps in
    loc, PCtor (i, ps)
  | Ast.PList ps ->
    List.fold_right (fun n acc -> let n = desugar_pat n in loc, PCons (n, acc)) ps (loc, PConst (loc, Udc (Str "[]")))
  | Ast.PTuple ps ->
    loc, PTuple (List.map desugar_pat ps)

(* desugar a given surface syntax construct to its core grammar equivalent *)
let rec desugar_expr ((loc, e) : Ast.located_expr): located_expr =
  let desugar_abs l r cons =
    let l = desugar_expr l in
    let r = desugar_expr r in
    cons l r
  in
  match e with
  | Ast.Const c -> loc, Const c
  | Ast.TypeLit t -> loc, TypeLit t
  | Ast.Binding (i, e) -> loc, Binding (i, desugar_expr e)
  | Ast.Bop (l, op, r) -> desugar_abs l r (fun l r -> loc, Bop (l, op, r))
  | Ast.Ap (b, l, r) -> desugar_abs l r (fun l r -> loc, Ap (b, l, r))
  | Ast.Pi (l, r) -> desugar_abs l r (fun l r -> loc, Pi (l, r))
  | Ast.List es ->
    List.fold_right (fun n acc -> let n = desugar_expr n in loc, Bop (n, Cons, acc)) es (loc, Const (loc, Udc (Str "[]")))
  | Ast.Tuple es -> loc, Tuple (List.map desugar_expr es)
  | Ast.Let (p, t, e, n) ->
    let p = desugar_pat p in
    let t = Base.Option.map ~f:desugar_expr t in
    let e = desugar_expr e in
    let n = desugar_expr n in
    loc, Let (p, t, e, n)
  | Ast.If (c, t, f) ->
    (* if statements are converted into 1/2 branch match statements *)
    let c = desugar_expr c in
    let t = desugar_expr t in
    let branches =
      let tp = loc, PConst (loc, Bool true) in
      let tb = [(tp, None, t)] in
      match f with
      | None -> tb
      | Some f ->
        let f = desugar_expr f in
        let fp = loc, PConst (loc, Bool false) in
        tb @ [(fp, None, f)]
    in loc, Match (c, branches)
  | Ast.Match (c, branches) ->
    let c = desugar_expr c in
    let branches =
      List.map (fun (cond, wb, b) -> (desugar_pat cond, Base.Option.map ~f:desugar_expr wb, desugar_expr b)) branches
    in loc, Match (c, branches)
  | Ast.Lam (ps, b) ->
    let b = desugar_expr b in
    (match ps with
    | [] -> raise (Error.InternalError "Internal error - no arguments to a lambda function.")
    | [ p ] ->
      let p = desugar_pat p in
      loc, Lam (p, b)
    | ps ->
      List.fold_left (fun acc n -> let n = desugar_pat n in loc, Lam (n, acc)) b (List.rev ps))

let desugar_ty_decl ((loc, (i, decl)) : Ast.located_ty_decl) : located_ty_decl =
  let desugar_assoc ts = List.map (fun (i, e) -> i, desugar_expr e) ts in
  match decl with
  | Ast.Alias t -> loc, (i, Alias (desugar_expr t))
  | Ast.Variant ts -> loc, (i, Variant (desugar_assoc ts))
  | Ast.Record ts -> loc, (i, Record (desugar_assoc ts))

let rec desugar_def ((loc, d) : Ast.located_definition) : located_definition =
  match d with  
  | Ast.Dec (i, e) -> loc, Dec (i, desugar_expr e)
  | Ast.Def (i, args, when_block, b, with_block) ->
    let args = List.map desugar_pat args in
    let when_block = Base.Option.map ~f:desugar_expr when_block in
    let b = desugar_expr b in
    let with_block = List.map desugar_def with_block in
    loc, Def (i, args, when_block, b, with_block)

(*
  this desugars function-level pattern matching in the style of:
    dec map : (A -> B) -> [A] -> [B]
    def map _ [] := []
    def map f (x :: xs) := f x :: map f xs

  turning it to:
    dec map : (A -> B) -> [A] -> [B]
    def map __match__1 __match__2 :=
      match (__match__1, __match__2) to
      | (_, []) => []
      | (f, x :: xs) => f x :: map f xs
*)
and desugar_flpm (loc, (i, args, when_block, body, wb)) (defs : located_definition list) =
  let defs', matches =
    let args_as_styles = List.map pattern_to_style args in
    let rec group_defs ds failed_acc acc =
      match ds with
      | (_, Dec _) as d :: ds -> group_defs ds (d :: failed_acc) acc
      | (loc, Def (i', args', wb, b, wb')) as failed :: ds ->
        let success = (loc, (i', args', wb, b, wb')) in
        let equal_i = get_str_combine i = get_str_combine i' in
        let equal_arg_c = List.length args = List.length args' in
        if equal_i && equal_arg_c
        then (
          (* if the functions have the same identifier and argument count, we check that their argument styles match. *)
          let args'' = List.map pattern_to_style args in
          if List.map2 ( $= ) args'' args_as_styles |> List.fold_left ( && ) true
          (* any successful matches are removed from the definition list so that they aren't checked again. *)
          then group_defs ds failed_acc (success :: acc) 
          else group_defs ds (failed :: failed_acc) acc
        )
        else group_defs ds (failed :: failed_acc) acc
      | [] -> failed_acc, acc
    in group_defs defs [] []
  in
  match matches with
  | [] -> (loc, Def (i, args, when_block, body, wb)), defs
  | _ ->
    let match_idents = List.map (fun _ -> loc, Ident (fresh_pattern_ident ())) args in
    let new_body, with_block =
      let rec construct_branches ds branch_acc wb_acc = 
        match ds with
        | [] -> 
          let b = ((loc, PTuple args), when_block, body) in
          b :: branch_acc, List.flatten (wb :: wb_acc)
        | (_, (_, args, when_block, b, with_block)) :: ds ->
          (* map f (x :: xs) : ... = ... ==> | (f, (x :: xs)) when ... => ... *)
          let branch = ((loc, PTuple args), when_block, b) in
          construct_branches ds (branch :: branch_acc) (with_block :: wb_acc) (* with_blocks are preserved to prevent any undefined variable errors *)
      in
      let c = loc, Tuple (List.map (fun i -> loc, Const i) match_idents) in
      let branches, wb = construct_branches matches [] [] in
      (loc, Match (c, branches)), wb
    in
    (* turns the args of the top-level function to the placeholder variables *)
    let args = List.map (fun i -> loc, PConst i) match_idents in
    let new_def = loc, (Def (i, args, when_block, new_body, with_block)) in
    new_def, defs'

let desugar_program ((i, imps, decls, defs) : Ast.program) : program = 
  let defs = 
    let defs = List.map desugar_def defs in
    let rec without_matches ds acc =
      match ds with
      | [] -> acc
      | (_, Dec _) as d :: ds -> without_matches ds (d :: acc)
      | (loc, Def (i, args, when_block, body, with_block)) :: ds ->
        let d, ds = desugar_flpm (loc, (i, args, when_block, body, with_block)) ds in
        without_matches ds (d :: acc)
    in without_matches defs []
  in
  i, imps, List.map desugar_ty_decl decls, defs

(* pretty printing *)
let rec pp_pattern out ((_, arg) : located_pattern) =
  match arg with
  | PConst c -> pp_const out c
  | PWild -> Format.fprintf out "_"
  | PCons (l, r) -> Format.fprintf out "(:: @[<hov>%a %a@])" pp_pattern l pp_pattern r
  | PCtor (i, v) ->
    Format.fprintf
      out
      "(%a %a)"
      pp_ident
      i
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") pp_pattern)
      v
  | PTuple ps ->
    Format.fprintf
      out
      "(@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_pattern)
      ps
;;

let rec pp_expr out ((_, e) : located_expr) =
  match e with
  | Const c -> pp_const out c
  | Ap (_, f, arg) -> Format.fprintf out "(%@ @[<hov>%a@ %a@])" pp_expr f pp_expr arg
  | Bop (l, op, r) ->
    Format.fprintf out "(@[<hov>%a@ %a@ %a@])" pp_binop op pp_expr l pp_expr r
  | Tuple t ->
    Format.fprintf
      out
      "(@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_expr)
      t
  | Let (p, ty, v, n) ->
    Format.fprintf
      out
      "@[<v>(@[<hov>%a@ %a@ %a@])@,%a@]"
      pp_pattern
      p
      Format.(pp_print_option ~none:(fun out () -> fprintf out "<none>") pp_expr)
      ty
      pp_expr
      v
      pp_expr
      n
  | Lam (arg, body) ->
    Format.fprintf
      out
      "(la@[<v>m (%a) %a@])"
      pp_pattern arg
      pp_expr body
  | Match (cond, bs) ->
    let pp_branch out (p, wb, b) =
      Format.fprintf
        out
        "(wh@[<v>en %a@,(%a %a)@])"
        Format.(pp_print_option ~none:(fun out () -> fprintf out "true") pp_expr)
        wb
        pp_pattern
        p
        pp_expr
        b
    in
    Format.fprintf
      out
      "(ma@[<v>tch (%a)@,%a@])"
      pp_expr
      cond
      Format.(pp_print_list ~pp_sep:pp_print_cut pp_branch)
      bs
  | Pi (l, r) -> Format.fprintf out "(%a -> %a)" pp_expr l pp_expr r
  | Binding (i, e) -> Format.fprintf out "(%a : %a)" pp_ident i pp_expr e
  | TypeLit p -> Format.fprintf out "%a" pp_prim p
;;

let rec pp_ty_decl out ((_, (i, t)) : located_ty_decl) =
  match t with
  | Alias _ ->
    Format.fprintf
      out
      "(ty@[<v>pe %a %a@])"
      pp_ident
      i
      pp_tdecl_type
      t
  | _ -> Format.fprintf out "(ty@[<v>pe %a@,%a@])" pp_ident i pp_tdecl_type t

and pp_tdecl_type out (t : tdecl_type) =
  let pp_field out ((i, t) : ident * located_expr) =
    Format.fprintf out "(%a %a)" pp_ident i pp_expr t
  in
  match t with
  | Alias t -> pp_expr out t
  | Record r ->
    Format.fprintf
      out
      "(re@[<v>cord@,%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_field)
      r
  | Variant v ->
    Format.fprintf
      out
      "(va@[<v>riant@,%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_field)
      v
;;

let pp_when_block out (when_block : located_expr option) =
  Format.fprintf
    out
    "%a"
    Format.(
      pp_print_option
        ~none:(fun out () -> fprintf out "()")
        (fun out block -> fprintf out "(when @[<hov>%a@])" pp_expr block))
    when_block
;;

let rec pp_definition out ((_, def) : located_definition) =
  match def with
  | Dec (f, ts) -> Format.fprintf out "(dec %a @[<hov>%a@])" pp_ident f pp_expr ts
  | Def (f, args, when_block, body, with_block) ->
    Format.fprintf
      out
      "(de@[<v>f %a (%a)@,%a@,%a@,%a@])"
      pp_ident
      f
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") pp_pattern)
      args
      pp_when_block
      when_block
      pp_expr
      body
      pp_with_block
      with_block

and pp_with_block out (with_block : with_block) =
  let block out () =
    match with_block with
    | [] -> Format.fprintf out "<none>"
    | _ ->
      Format.fprintf
        out
        "%a"
        Format.(pp_print_list ~pp_sep:pp_print_cut pp_definition)
        with_block
  in
  Format.fprintf out "(wi@[<v>th@,%a@])" block ()
;;

let pp_module out (mod_name : ident) = Format.fprintf out "(module %a)" pp_ident mod_name

let pp_program out ((prog_name, imports, types, body) : program) =
  Format.fprintf
    out
    "%a@.@.%a@.@.%a@.@.%a@."
    pp_module
    prog_name
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_import)
    imports
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_ty_decl)
    types
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_definition)
    body
;;
