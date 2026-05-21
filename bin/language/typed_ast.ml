open Util
open Primitive

type located_pattern = Location.t * pattern

and pattern =
  | PWild (* _ *)
  | PConst of located_const
  | PBop of located_pattern * ident * located_pattern
  | PCtor of ident * located_pattern list
  | PTuple of located_pattern list

type typed_expr = located_expr * located_expr
and located_expr = Location.t * expr

and expr =
  | Const of located_const
  | Ap of binder * typed_expr * typed_expr
  | Tuple of typed_expr list
  | Let of located_pattern * typed_expr * typed_expr
  | Lam of located_pattern * typed_expr
  | Match of typed_expr * (located_pattern * typed_expr option * typed_expr) list
  | TypeLit of prim
  | Binding of ident * typed_expr (* x : T *)
  | Pi of typed_expr * typed_expr

type located_ty_decl = Location.t * ty_decl
and ty_decl = ident * tdecl_type

and tdecl_type =
  | Alias of typed_expr
  | Variant of typed_expr * (ident * typed_expr) list
  | Record of ident * typed_expr * (ident * typed_expr) list

type located_definition = Location.t * definition

and definition =
  ident
  * typed_expr (* function type *)
  * located_pattern list (* args *)
  * typed_expr option (* optional when-block *)
  * typed_expr (* function body *)

type program =
  ident * located_import list * located_ty_decl list * located_definition list

(* utils *)
let rec show_pat = function
  | _, PWild -> "_"
  | _, PBop (l, op, r) -> Printf.sprintf "%s %s %s" (show_pat l) (get_str op) (show_pat r)
  | _, PCtor (n, p) ->
    Format.asprintf "%a %s" pp_ident n (List.map show_pat p |> String.concat " ")
  | _, PTuple ps -> Printf.sprintf "(%s)" (List.map show_pat ps |> String.concat ", ")
  | _, PConst c -> Format.asprintf "%a" pp_const c
;;

let rec convert ((loc, p) : Elab.located_pattern) : located_pattern =
  let p = match p with
    | Elab.PWild -> PWild
    | Elab.PConst c -> PConst c
    | Elab.PBop (l, op, r) -> PBop (convert l, op, convert r)
    | Elab.PCtor (i, cs) -> PCtor (i, List.map convert cs)
    | Elab.PTuple ps -> PTuple (List.map convert ps)
  in loc, p

(* pretty printing *)
let rec pp_pattern out ((_, arg) : located_pattern) =
  match arg with
  | PConst c -> pp_const out c
  | PWild -> Format.fprintf out "_"
  | PBop (l, op, r) -> Format.fprintf out "(%a @[<hov>%a %a@])" pp_ident op pp_pattern l pp_pattern r
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
  | Ap (_, f, arg) ->
    Format.fprintf out "(@[<hov>%a@ %a@])" pp_typed_expr f pp_typed_expr arg
  | Tuple t ->
    Format.fprintf
      out
      "(@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_typed_expr)
      t
  | Let (p, v, n) ->
    Format.fprintf
      out
      "(le@[<v>t %a %a@,%a@])"
      pp_pattern
      p
      pp_typed_expr
      v
      pp_typed_expr
      n
  | Lam (arg, body) ->
    Format.fprintf out "(la@[<v>m (%a)@,%a@])" pp_pattern arg pp_typed_expr body
  | Match (cond, bs) ->
    let pp_branch out (p, wb, b) =
      Format.fprintf
        out
        "(wh@[<v>en %a@,%a %a@])"
        Format.(pp_print_option ~none:(fun out () -> fprintf out "true") pp_typed_expr)
        wb
        pp_pattern
        p
        pp_typed_expr
        b
    in
    Format.fprintf
      out
      "(ma@[<v>tch (%a)@,%a@])"
      pp_typed_expr
      cond
      Format.(pp_print_list ~pp_sep:pp_print_cut pp_branch)
      bs
  | Pi (l, r) -> Format.fprintf out "(%a -> %a)" pp_typed_expr l pp_typed_expr r
  | Binding (i, e) -> Format.fprintf out "(%a : %a)" pp_ident i pp_typed_expr e
  | TypeLit p -> Format.fprintf out "%a" pp_prim p

and pp_typed_expr out ((t, e) : typed_expr) =
  match e with
  | _, Let (_, _, _) | _, Match (_, _) | _, Lam (_, _) -> pp_expr out e
  | _ -> Format.fprintf out "(%a %a)" pp_expr t pp_expr e
;;

let rec pp_ty_decl out ((_, (i, t)) : located_ty_decl) =
  match t with
  | Alias _ -> Format.fprintf out "(ty@[<v>pe %a %a@])" pp_ident i pp_tdecl_type t
  | _ -> Format.fprintf out "(ty@[<v>pe %a@,%a@])" pp_ident i pp_tdecl_type t

and pp_tdecl_type out (t : tdecl_type) =
  let pp_field out ((i, t) : ident * typed_expr) =
    Format.fprintf out "(%a %a)" pp_ident i pp_typed_expr t
  in
  match t with
  | Alias t -> pp_typed_expr out t
  | Record (cons, ty, r) ->
    Format.fprintf
      out
      "(re@[<v>cord %a { %a }@,%a@])"
      pp_ident cons
      pp_typed_expr ty
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_field)
      r
  | Variant (ty, v) ->
    Format.fprintf
      out
      "(va@[<v>riant { %a }@,%a@])"
      pp_typed_expr ty
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_field)
      v
;;

let pp_when_block out (when_block : typed_expr option) =
  Format.fprintf
    out
    "%a"
    Format.(
      pp_print_option
        ~none:(fun out () -> fprintf out "()")
        (fun out block -> fprintf out "(when @[<hov>%a@])" pp_typed_expr block))
    when_block
;;

let pp_module out (mod_name : ident) = Format.fprintf out "(module %a)" pp_ident mod_name

let pp_typed_definition out ((_, (f, ret, args, when_block, body)) : located_definition) =
  Format.fprintf
    out
    "(de@[<v>f %a (%a)@,(%a)@,%a@,%a@])"
    pp_ident
    f
    pp_typed_expr
    ret
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") pp_pattern)
    args
    pp_when_block
    when_block
    pp_typed_expr
    body
;;

let pp_typed_program out ((prog_name, imports, types, body) : program) =
  Format.fprintf
    out
    "%a@.@.%a@.@.%a@.@.%a@."
    pp_module
    prog_name
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_import)
    imports
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_ty_decl)
    types
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_typed_definition)
    body
;;
