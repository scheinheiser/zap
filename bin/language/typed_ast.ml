open Util

type ident = string
type func = string
type module_name = string
type tname = string
type binder = int

type typed_expr = Ast.located_ty * located_expr
and located_expr = Location.t * expr

and expr =
  | Const of Ast.located_const
  | EList of typed_expr list
  | ETup of typed_expr list
  | Bop of typed_expr * Ast.binop * typed_expr
  | Ap of binder * typed_expr * typed_expr
  | Let of Ast.located_pattern * typed_expr * typed_expr
  | Grouping of typed_expr
  | If of typed_expr * typed_expr * typed_expr option
  | Lam of Ast.located_pattern list * typed_expr

type located_definition = Location.t * definition

and definition =
  bool * func * Ast.located_ty * Ast.located_pattern list * typed_expr option * typed_expr

type program =
  module_name
  * Ast.located_import list
  * Ast.located_ty_decl list
  * located_definition list

let rec pp_expr out ((_, e) : located_expr) =
  match e with
  | Const c -> Ast.pp_const out c
  | ETup t ->
    Format.fprintf
      out
      "(@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ") pp_typed_expr)
      t
  | EList l ->
    Format.fprintf
      out
      "[@[<hov>%a@]]"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_typed_expr)
      l
  | Ap (_, f, arg) ->
    Format.fprintf out "(%@ @[<hov>%a@ %a@])" pp_typed_expr f pp_typed_expr arg
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
  | Grouping body -> Format.fprintf out "(@[<v>%a@])" pp_typed_expr body
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
  | Lam (args, body) ->
    Format.fprintf
      out
      "(la@[<v>m (%a)@,%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") Ast.pp_pattern)
      args
      pp_typed_expr
      body

and pp_typed_expr out ((t, expr) : typed_expr) =
  match expr with
  | _, Let _ | _, Grouping _ | _, If _ | _, Lam _ -> Format.fprintf out "%a" pp_expr expr
  | _ -> Format.fprintf out "(%a %a)" Ast.pp_ty t pp_expr expr
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
    "(de@[<v>f %s (%a)@,(%a)@,%a@,%a@])"
    f
    Ast.pp_ty
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
