open Util
open Primitive

(* desugared syntax *)
type located_pattern = Location.t * pattern

and pattern =
  | PWild (* _ *)
  | PConst of located_const
  | PCons of located_pattern * located_pattern
  | PCtor of ident * located_pattern list

type located_expr = Location.t * expr

and expr =
  | Bop of located_expr * binop * located_expr
  | Ap of binder * located_expr * located_expr
  (* we give each function a binder to distinguish between user-defined functions and builtins later on *)
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

type import_cond =
  | CWith of ident list
  | CWithout of ident list

type located_import = Location.t * import
and import = ident * import_cond option

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

type top_lvl =
  | TDef of located_definition
  | TTyDecl of located_ty_decl
  | TImport of located_import

type program =
  ident * located_import list * located_ty_decl list * located_definition list

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
    List.fold_right (fun n acc -> let n = desugar_pat n in loc, PCons (n, acc)) ps (loc, PConst (loc, String "empty list"))

(* desugar a given surface syntax construct to its core grammar equivalent *)
let rec desugar_expr ((loc, e) : Ast.located_expr): located_expr =
  match e with
  | Ast.Const c -> loc, Const c
  | Ast.TypeLit t -> loc, TypeLit t
  | Ast.Binding (i, e) ->
    let e = desugar_expr e in
    loc, Binding (i, e)
  | Ast.Bop (l, op, r) ->
    let l = desugar_expr l in
    let r = desugar_expr r in
    loc, Bop (l, op, r)
  | Ast.Ap (b, l, r) ->
    let l = desugar_expr l in
    let r = desugar_expr r in
    loc, Ap (b, l, r)
  | Ast.Pi (l, r) ->
    let l = desugar_expr l in
    let r = desugar_expr r in
    loc, Pi (l, r)
  | Ast.List es ->
    List.fold_right (fun n acc -> let n = desugar_expr n in loc, Bop (n, Cons, acc)) es (loc, Const (loc, String "empty list"))
  | Ast.Let (p, t, e, n) ->
    let p = desugar_pat p in
    let t = Base.Option.map ~f:desugar_expr t in
    let e = desugar_expr e in
    let n = desugar_expr n in
    loc, Let (p, t, e, n)
  | Ast.If (c, t, f) ->
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
;;

let rec pp_expr out ((_, e) : located_expr) =
  match e with
  | Const c -> pp_const out c
  | Ap (_, f, arg) -> Format.fprintf out "(%@ @[<hov>%a@ %a@])" pp_expr f pp_expr arg
  | Bop (l, op, r) ->
    Format.fprintf out "(@[<hov>%a@ %a@ %a@])" pp_binop op pp_expr l pp_expr r
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
        "(wh@[<v>en %a@,%a %a@])"
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

let pp_import_cond out (cond : import_cond) =
  match cond with
  | CWith includes ->
    Format.fprintf
      out
      "with (@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") pp_ident)
      includes
  | CWithout excludes ->
    Format.fprintf
      out
      "without (@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") pp_ident)
      excludes
;;

let pp_import out ((_, (mod_name, cond)) : located_import) =
  Format.fprintf
    out
    "(import %a @[<hov>%a@])"
    pp_ident
    mod_name
    Format.(pp_print_option ~none:(fun out () -> fprintf out "()") pp_import_cond)
    cond
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
