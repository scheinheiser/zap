open Util
(* made with occasional referencing of https://github.com/camllight/camllight/blob/master/sources/src/compiler/syntax.ml *)

(* Type definitions *)
type ident = string
type func = string
type module_name = string
type tname = string
type binder = int

type prim =
  | PInt
  | PFloat
  | PString
  | PChar
  | PBool
  | PAtom
  | PUnit
  | PGeneric of string

type located_ty = Location.t * ty

and ty =
  | Arrow of located_ty * located_ty
  | List of located_ty
  | Constructor of tname * located_ty
  | Tuple of located_ty list
  | Prim of prim
  | Udt of tname (* user defined type *)

type located_const = Location.t * const

and const =
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool
  | Atom of ident
  | Unit

type binop =
  | Add
  | Mul
  | Sub
  | Div
  | Less
  | Greater
  | LessE
  | GreaterE
  | Equal
  | NotEq
  | And
  | Or
  | Cons
  | User_op of ident

type located_expr = Location.t * expr

and expr =
  | Const of located_const
  | EList of located_expr list
  | Ident of ident
  | Bop of located_expr * binop * located_expr
  | Ap of binder * located_expr * located_expr
(* we give each function a binder to distinguish between user-defined functions and builtins later on *)
  | ETup of located_expr list

type located_pattern = Location.t * pattern

and pattern =
  | PConst of located_const
  | PIdent of ident
  | PWild (* wildcard, '_' *)
  | PCons of located_pattern * located_pattern
  | PList of located_pattern list
  | PTup of located_pattern list

type located_term = Location.t * term

and term =
  | TExpr of located_expr
  | TLet of ident * located_ty option * located_term
  | TGrouping of located_term list
  | TIf of located_expr * located_term * located_term option
  | TLam of located_pattern list * located_term

type import_cond =
  | CWith of ident list
  | CWithout of ident list

type located_import = Location.t * import
and import = module_name * import_cond option

type located_ty_decl = Location.t * ty_decl
and ty_decl = ident * tdecl_type

and tdecl_type =
  | Alias of located_ty
  | Variant of (ident * located_ty) list
  | Record of (ident * located_ty) list

type located_definition = Location.t * definition

and definition =
  | Dec of func * located_ty
  | Def of
      func
      * located_pattern list
      * located_term option
      * located_term list
      * with_block option
(* identifer, args, optional when-block, body, optional with-block *)

and with_block = located_definition list

type top_lvl =
  | TDef of located_definition
  | TTyDecl of located_ty_decl
  | TImport of located_import

type program =
  module_name * located_import list * located_ty_decl list * located_definition list

(* utils *)
let show_prim = function
  | PInt -> "int"
  | PFloat -> "float"
  | PString -> "string"
  | PChar -> "char"
  | PBool -> "bool"
  | PAtom -> "atom"
  | PUnit -> "unit"
  | PGeneric g -> g
;;

let rec show_ty = function
  | Arrow ((_, l), (_, r)) -> Printf.sprintf "%s -> %s" (show_ty l) (show_ty r)
  | List (_, t) -> Printf.sprintf "[%s]" (show_ty t)
  | Constructor (c, (_, t)) -> Printf.sprintf "%s %s" c (show_ty t)
  | Tuple ts ->
    let ts' = List.map (fun (_, t) -> show_ty t) ts in
    "(" ^ String.concat ", " ts' ^ ")"
  | Prim p -> show_prim p
  | Udt t -> t
;;

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
    | PUnit -> "unit"
    | PGeneric n -> n
  in
  Format.fprintf out "%s" (of_prim t)
;;

let rec pp_ty out ((_, ty) : located_ty) =
  match ty with
  | Arrow (l, r) -> Format.fprintf out "(@[<hov>->@ %a@ %a@])" pp_ty l pp_ty r
  | List t -> Format.fprintf out "[%a]" pp_ty t
  | Constructor (c, t) -> Format.fprintf out "(@[<hov>%s@ %a@])" c pp_ty t
  | Tuple ts ->
    Format.fprintf
      out
      "(@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_ty)
      ts
  | Prim p -> pp_prim out p
  | Udt t -> pp_ident out t
;;

let pp_const out ((_, c) : located_const) =
  match c with
  | Int i -> Format.fprintf out "%d" i
  | Float f -> Format.fprintf out "%.5f" f
  | String s -> Format.fprintf out "\"%s\"" s
  | Char c' -> Format.fprintf out "'%c'" c'
  | Bool b -> Format.fprintf out "%b" b
  | Atom a -> Format.fprintf out "%@%s" a
  | Unit -> Format.fprintf out "()"
;;

let pp_binop out (b : binop) =
  let op =
    match b with
    | Add -> "+"
    | Mul -> "*"
    | Sub -> "-"
    | Div -> "/"
    | Less -> "<"
    | Greater -> ">"
    | LessE -> "<="
    | GreaterE -> ">="
    | Equal -> "="
    | NotEq -> "/="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"
    | User_op o -> o
  in
  Format.fprintf out "%s" op
;;

let rec pp_expr out ((_, e) : located_expr) =
  match e with
  | Const c -> pp_const out c
  | Ident i -> pp_ident out i
  | ETup t ->
    Format.fprintf
      out
      "(@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ") pp_expr)
      t
  | EList l ->
    Format.fprintf
      out
      "[@[<hov>%a@]]"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_expr)
      l
  | Ap (_, f, arg) -> Format.fprintf out "(%@ @[<hov>%a@ %a@])" pp_expr f pp_expr arg
  | Bop (l, op, r) ->
    Format.fprintf out "(@[<hov>%a@ %a@ %a@])" pp_binop op pp_expr l pp_expr r
;;

let rec pp_pattern out ((_, arg) : located_pattern) =
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
  | PTup ps ->
    Format.fprintf
      out
      "(@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_pattern)
      ps
  | PCons (l, r) -> Format.fprintf out "(:: @[<hov>%a %a@])" pp_pattern l pp_pattern r
;;

let rec pp_term out ((_, t) : located_term) =
  match t with
  | TExpr e -> pp_expr out e
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

let pp_import out ((_, (mod_name, cond)) : located_import) =
  Format.fprintf
    out
    "(import %s @[<hov>%a@])"
    mod_name
    Format.(pp_print_option ~none:(fun out () -> fprintf out "()") pp_import_cond)
    cond
;;

let pp_tdecl_type out (t : tdecl_type) =
  match t with
  | Alias t -> pp_ty out t
  | Record r ->
    let pp_field out ((i, t) : ident * located_ty) =
      Format.fprintf out "(%s %a)" i pp_ty t
    in
    Format.fprintf
      out
      "(re@[<v>cord@,%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_field)
      r
  | Variant v ->
    let pp_field out ((i, t) : ident * located_ty) =
      Format.fprintf out "(%s %a)" i pp_ty t
    in
    Format.fprintf
      out
      "(va@[<v>riant@,%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@,") pp_field)
      v
;;

let pp_ty_decl out ((_, (i, t)) : located_ty_decl) =
  match t with
  | Alias _ ->
    Format.fprintf
      out
      "(ty@[<v>pe %s %a@])"
      i
      pp_tdecl_type
      t (* it looks nicer on a single line *)
  | _ -> Format.fprintf out "(ty@[<v>pe %s@,%a@])" i pp_tdecl_type t
;;

let pp_when_block out (when_block : located_term option) =
  Format.fprintf
    out
    "%a"
    Format.(
      pp_print_option
        ~none:(fun out () -> fprintf out "()")
        (fun out block -> fprintf out "(when @[<hov>%a@])" pp_term block))
    when_block
;;

let rec pp_definition out ((_, def) : located_definition) =
  match def with
  | Dec (f, ts) -> Format.fprintf out "(dec %s @[<hov>%a@])" f pp_ty ts
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
