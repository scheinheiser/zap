open Util

type ident = string
type func = string
type module_name = string
type tname = string

type typed_expr = Ast.located_ty * located_expr
and located_expr = Location.t * expr

and expr =
  | Const of Ast.located_const
  | EList of typed_expr list
  | Ident of ident
  | Bop of typed_expr * Ast.binop * typed_expr
  | Ap of typed_expr * typed_expr

type typed_term = Ast.located_ty * located_term
and located_term = Location.t * term

and term =
  | TExpr of typed_expr
  | TLet of ident * typed_term
  | TGrouping of typed_term list
  | TIf of typed_expr * typed_term * typed_term option
  | TTup of typed_expr list
  | TLam of Ast.located_pattern list * typed_term

type located_definition = Location.t * definition

and definition =
  | Dec of func * Ast.located_ty
  | Def of
      func
      * Ast.located_ty list
      * Ast.located_pattern list
      * typed_term option
      * typed_term list
      * with_block option

and with_block = located_definition list

type program =
  module_name
  * Ast.located_import list
  * Ast.located_ty_decl list
  * located_definition list

let rec pp_expr out ((_, e) : located_expr) =
  match e with
  | Const c -> Ast.pp_const out c
  | Ident i -> Ast.pp_ident out i
  | EList l ->
    Format.fprintf
      out
      "[@[<hov>%a@]]"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_typed_expr)
      l
  | Ap (f, arg) -> Format.fprintf out "(%@ @[<hov>%a@ %a@])" pp_typed_expr f pp_typed_expr arg
  | Bop (l, op, r) ->
    Format.fprintf out "(@[<hov>%a@ %a@ %a@])" Ast.pp_binop op pp_typed_expr l pp_typed_expr r
and pp_typed_expr out ((t, expr) : typed_expr) =
  Format.fprintf out "(%a %a)" Ast.pp_ty t pp_expr expr

let rec pp_term out ((_, t) : located_term) =
  match t with
  | TExpr e -> pp_typed_expr out e
  | TTup t ->
    Format.fprintf
      out
      "(@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ") pp_typed_expr)
      t
  | TLet (i, v) ->
    Format.fprintf
      out
      "(@[<hov>%s@ %a@])"
      i
      pp_typed_term
      v
  | TGrouping body ->
    Format.fprintf
      out
      "(@[<v>%a@])"
      Format.(pp_print_list ~pp_sep:pp_print_cut pp_typed_term)
      body
  | TIf (cond, tbranch, fbranch) ->
    Format.fprintf
      out
      "(if@[<v> %a@,%a@,%a@])"
      pp_typed_expr
      cond
      pp_typed_term
      tbranch
      Format.(pp_print_option ~none:(fun out () -> fprintf out "<none>") pp_typed_term)
      fbranch
  | TLam (args, body) ->
    Format.fprintf
      out
      "(la@[<v>m (%a)@,%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") Ast.pp_pattern)
      args
      pp_typed_term
      body

and pp_typed_term out ((_, term) : typed_term) =
  Format.fprintf out "%a" pp_term term

let pp_when_block out (when_block : typed_term option) =
  Format.fprintf
    out
    "%a"
    Format.(
      pp_print_option
        ~none:(fun out () -> fprintf out "()")
        (fun out block -> fprintf out "(when @[<hov>%a@])" pp_typed_term block))
    when_block
;;

let rec pp_typed_definition out ((_, def) : located_definition) =
  match def with
  | Dec (f, ts) -> Format.fprintf out "(dec %s @[<hov>%a@])" f Ast.pp_ty ts
  | Def (f, ret, args, when_block, body, with_block) ->
    Format.fprintf
      out
      "(de@[<v>f %s (%a)@,(%a)@,%a@,%a@,%a@])"
      f
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") Ast.pp_ty)
      ret
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") Ast.pp_pattern)
      args
      pp_when_block
      when_block
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_typed_term)
      body
      pp_with_block
      with_block
and pp_with_block out (with_block : with_block option) =
  Format.fprintf
    out
    "(wi@[<v>th@,%a@])"
    Format.(
      pp_print_option
        ~none:(fun out () -> fprintf out "<none>")
        (pp_print_list ~pp_sep:pp_print_cut pp_typed_definition))
    with_block
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
