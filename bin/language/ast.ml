(* made with occasional referencing of https://github.com/camllight/camllight/blob/master/sources/src/compiler/syntax.ml *)

(* Type definitions *)
type ident = string
type func = string
type module_name = string
type tname = string

type prim =
  | PInt
  | PFloat
  | PString
  | PChar
  | PBool
  | PAtom
  | PUnit
  | PGeneric of string

type ty =
  | Arrow of ty * ty
  | List of ty
  | Constructor of tname * ty list
  | Tuple of ty list
  | Prim of prim
  | Udt of tname (* user defined type *)

type const =
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool
  | Atom of ident
  | Unit

type expr =
  | Const of const
  | EList of expr list
  | Ident of ident
  | Bop of expr * ident * expr
  | Ap of expr * expr

type pattern =
  | PConst of const
  | PIdent of ident
  | PWild (* wildcard, '_' *)
  | PCons of pattern * pattern
  | PList of pattern list

type term =
  | TExpr of expr
  | TLet of ident * ty option * term
  | TGrouping of term list
  | TIf of expr * term * term option
  | TTup of expr list
  | TLam of pattern list * term

type import_cond =
  | CWith of ident list
  | CWithout of ident list

type import = module_name * import_cond option

type ty_decl = ident * tdecl_type

and tdecl_type =
  | Alias of ty
  | Variant of (ident * ty option) list
  | Record of (ident * ty) list

type definition =
  | Dec of func * ty list
  | Def of func * pattern list * term option * term list * with_block option
(* identifer, args, optional when-block, body, optional with-block *)

and with_block = definition list

type top_lvl =
  | TDef of definition
  | TTyDecl of ty_decl
  | TImport of import

type program = module_name * import list * ty_decl list * definition list

(* Pretty printing *)
let pp_ident out (i : ident) = Format.fprintf out "%s" i

let pp_prim out (t : prim) =
  let of_prim = function
    | PInt -> "int"
    | PFloat -> "float"
    | PString -> "string"
    | PChar -> "char"
    | PBool -> "bool"
    | PAtom -> "atom"
    | PUnit -> "()"
    | PGeneric n -> n
  in
  Format.fprintf out "%s" (of_prim t)
;;

let rec pp_ty out (ty' : ty) =
  match ty' with
  | Arrow (l, r) -> Format.fprintf out "(@[<hov>->@ %a@ %a@])" pp_ty l pp_ty r
  | List t -> Format.fprintf out "[%a]" pp_ty t
  | Constructor (c, ts) ->
    Format.fprintf
      out
      "(@[<hov>%s@ %a@])"
      c
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ") pp_ty)
      ts
  | Tuple ts ->
    Format.fprintf
      out
      "(@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_ty)
      ts
  | Prim p -> pp_prim out p
  | Udt t -> pp_ident out t
;;

let pp_const out (c : const) =
  match c with
  | Int i -> Format.fprintf out "%d" i
  | Float f -> Format.fprintf out "%.5f" f
  | String s -> Format.fprintf out "%s\"" s
  | Char c' -> Format.fprintf out "'%c'" c'
  | Bool b -> Format.fprintf out "%b" b
  | Atom a -> Format.fprintf out "%@%s" a
  | Unit -> Format.fprintf out "()"
;;

let rec pp_expr out (e : expr) =
  match e with
  | Const c -> pp_const out c
  | Ident i -> pp_ident out i
  | EList l ->
    Format.fprintf
      out
      "[@[<hov>%a@]]"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_expr)
      l
  | Ap (f, arg) -> Format.fprintf out "(%@ @[<hov>%a@ %a@])" pp_expr f pp_expr arg
  | Bop (l, op, r) -> Format.fprintf out "(@[<hov>%s@ %a@ %a@])" op pp_expr l pp_expr r
;;

let rec pp_pattern out (arg : pattern) =
  match arg with
  | PConst c -> pp_const out c
  | PIdent i -> pp_ident out i
  | PWild -> Format.fprintf out "_"
  | PList ps ->
    Format.fprintf
      out
      "[@[<hov>%a@]]"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_pattern)
      ps
  | PCons (l, r) -> Format.fprintf out "(:: @[<hov>%a %a@])" pp_pattern l pp_pattern r
;;

let rec pp_term out (t : term) =
  match t with
  | TExpr e -> pp_expr out e
  | TTup t ->
    Format.fprintf
      out
      "(@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ") pp_expr)
      t
  | TLet (i, ty, v) ->
    Format.fprintf
      out
      "(@[<hov>%s@ %a@ %a@])"
      i
      Format.(pp_print_option ~none:(fun out () -> fprintf out "<none>") pp_ty)
      ty
      pp_term
      v
  | TGrouping body ->
    Format.fprintf
      out
      "(@[<v>%a@])"
      Format.(pp_print_list ~pp_sep:pp_print_cut pp_term)
      body
  | TIf (cond, tbranch, fbranch) ->
    Format.fprintf
      out
      "(if @[<v>%a@,%a@,%a@])"
      pp_expr
      cond
      pp_term
      tbranch
      Format.(pp_print_option ~none:(fun out () -> fprintf out "<none>") pp_term)
      fbranch
  | TLam (args, body) ->
    Format.fprintf
      out
      "(lam @[<v>(%a) %a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ") pp_pattern)
      args
      pp_term
      body
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

let pp_import out ((mod_name, cond) : import) =
  Format.fprintf
    out
    "(import %s @[<hov>%a@])"
    mod_name
    Format.(pp_print_option ~none:(fun out () -> fprintf out "()") pp_import_cond)
    cond
;;

(* type ty_decl = ident * tdecl_type *)
let pp_tdecl_type out (t : tdecl_type) =
  match t with
  | Alias t -> pp_ty out t
  | Record r ->
    let pp_field out ((i, t) : ident * ty) = Format.fprintf out "(%s %a)" i pp_ty t in
    Format.fprintf
      out
      "(re@[<v>cord@,%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_field)
      r
  | Variant v ->
    let pp_field out ((i, t) : ident * ty option) =
      match t with
      | Some t' -> Format.fprintf out "(%s %a)" i pp_ty t'
      | None -> Format.fprintf out "%s" i
    in
    Format.fprintf
      out
      "(va@[<v>riant@,%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_field)
      v
;;

let pp_ty_decl out ((i, t) : ty_decl) =
  Format.fprintf out "(ty@[<v>pe@ %s@ %a@])" i pp_tdecl_type t
;;

let pp_when_block out (when_block : term option) =
  Format.fprintf
    out
    "%a"
    Format.(
      pp_print_option
        ~none:(fun out () -> fprintf out "()")
        (fun out block -> fprintf out "(when @[<hov>%a@])" pp_term block))
    when_block
;;

let rec pp_definition out (def : definition) =
  match def with
  | Dec (f, ts) ->
    Format.fprintf
      out
      "(dec %s @[<hov>%a@])"
      f
      Format.(pp_print_list ~pp_sep:pp_print_space pp_ty)
      ts
  | Def (f, args, when_block, body, with_block) ->
    Format.fprintf
      out
      "(de@[<v>f %s (%a)@,%a@,%a@,%a@])"
      f
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") pp_pattern)
      args
      pp_when_block
      when_block
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_term)
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
        (pp_print_list ~pp_sep:pp_print_cut pp_definition))
    with_block
;;

let pp_module out (mod_name : module_name) = Format.fprintf out "(module %s)" mod_name

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
