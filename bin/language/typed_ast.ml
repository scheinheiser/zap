open Util

type binder = int

type typed_expr = located_expr * located_expr
and located_expr = Location.t * expr

and expr =
  | Const of Ast.located_const
  | List of typed_expr list
  | Bop of typed_expr * Ast.binop * typed_expr
  | Ap of binder * typed_expr * typed_expr
  | Let of Ast.located_pattern * typed_expr * typed_expr
  | If of typed_expr * typed_expr * typed_expr option
  | Lam of Ast.located_pattern * typed_expr
    (* `fun x y => x` is desugared to `fun x => fun y => x` *)
  | Match of typed_expr * (Ast.located_pattern * typed_expr option * typed_expr) list
  | TypeLit of Ast.prim
  | Binding of Ast.ident * typed_expr (* x : T *)
  | Pi of typed_expr * typed_expr

type located_definition = Location.t * definition

and definition =
  bool
  * Ast.ident
  * typed_expr
  * Ast.located_pattern list
  * typed_expr option
  * typed_expr

type program =
  Ast.ident * Ast.located_import list * Ast.located_ty_decl list * located_definition list

(* pretty printing *)

let rec pp_expr out ((_, e) : located_expr) =
  match e with
  | Const c -> Ast.pp_const out c
  | List l ->
    Format.fprintf
      out
      "([@[<hov>%a@]])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_typed_expr)
      l
  | Ap (_, f, arg) ->
    Format.fprintf out "(@[<hov>%a@ %a@])" pp_typed_expr f pp_typed_expr arg
  | Bop (l, op, r) ->
    Format.fprintf
      out
      "(@[<hov>%a@ %a@ %a@])"
      Ast.pp_binop
      op
      pp_typed_expr
      l
      pp_typed_expr
      r
  | Let (p, v, n) ->
    Format.fprintf
      out
      "(le@[<v>t %a %a@,%a@])"
      Ast.pp_pattern
      p
      pp_typed_expr
      v
      pp_typed_expr
      n
  | If (cond, tbranch, fbranch) ->
    Format.fprintf
      out
      "(if@[<v> %a@,%a@,%a@])"
      pp_typed_expr
      cond
      pp_typed_expr
      tbranch
      Format.(pp_print_option ~none:(fun out () -> fprintf out "<none>") pp_typed_expr)
      fbranch
  | Lam (arg, body) ->
    Format.fprintf
      out
      "(la@[<v>m (%a)@,%a@])"
      Ast.pp_pattern arg
      pp_typed_expr body
  | Match (cond, bs) ->
    let pp_branch out (p, wb, b) =
      Format.fprintf
        out
        "(wh@[<v>en %a@,%a %a@])"
        Format.(pp_print_option ~none:(fun out () -> fprintf out "true") pp_typed_expr)
        wb
        Ast.pp_pattern
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
  | Binding (i, e) -> Format.fprintf out "(%a : %a)" Ast.pp_ident i pp_typed_expr e
  | TypeLit p -> Format.fprintf out "%a" Ast.pp_prim p

and pp_typed_expr out ((t, e) : typed_expr) =
  match e with
  | _, If (_, _, _) | _, Let (_, _, _) | _, Match (_, _) | _, Lam (_, _) -> pp_expr out e
  | _ -> Format.fprintf out "(%a %a)" pp_expr t pp_expr e
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

let pp_typed_definition
      out
      ((_, (_, f, ret, args, when_block, body)) : located_definition)
  =
  Format.fprintf
    out
    "(de@[<v>f %a (%a)@,(%a)@,%a@,%a@])"
    Ast.pp_ident
    f
    pp_typed_expr
    ret
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") Ast.pp_pattern)
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
    Ast.pp_module
    prog_name
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") Ast.pp_import)
    imports
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") Ast.pp_ty_decl)
    types
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_typed_definition)
    body
;;
