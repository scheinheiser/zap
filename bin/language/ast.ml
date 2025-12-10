(* made with occasional referencing of https://github.com/camllight/camllight/blob/master/sources/src/compiler/syntax.ml *)
open Util

(* Type definitions *)
type ident = string
type func = string
type module_name = string
type tname = string
type binder = int

type located_const = Location.t * const

and const =
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool
  | Atom of ident
  | Unit
  | Ident of ident

type prim =
  | PInt
  | PFloat
  | PString
  | PChar
  | PBool
  | PAtom
  | PUnit
  | PUni of int (* A : Type n *)

type binop =
  | IAdd | FAdd
  | IMul | FMul
  | ISub | FSub
  | IDiv | FDiv
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
[@@ocamlformat "disable"]

type located_pattern = Location.t * pattern

and pattern =
  | PWild (* _ *)
  | PConst of located_const
  | PCons of located_pattern * located_pattern
  | PCtor of ident * located_pattern
  | PList of located_pattern list

type located_expr = Location.t * expr

and expr =
  | List of located_expr list
  | Bop of located_expr * binop * located_expr
  | Ap of binder * located_expr * located_expr
  (* we give each function a binder to distinguish between user-defined functions and builtins later on *)
  | Let of
      located_pattern
      * located_expr option
      * located_expr
      * located_expr (* let p₁ ... pₙ : <optional_ty> = e₁ in e₂ *)
  | Match of located_expr * (located_pattern * located_expr option * located_expr) list
  | If of located_expr * located_expr * located_expr option
  | Lam of located_pattern list * located_expr
  | Const of located_const
  | TypeLit of prim
  | Binding of string * located_expr (* x : T *)
  | Pi of located_expr * located_expr

type import_cond =
  | CWith of ident list
  | CWithout of ident list

type located_import = Location.t * import
and import = module_name * import_cond option

type located_ty_decl = Location.t * ty_decl
and ty_decl = ident * tdecl_type

and tdecl_type =
  | Alias of located_expr
  | Variant of (ident * located_expr) list
  | Record of (ident * located_expr) list

type located_definition = Location.t * definition

and definition =
  | Dec of bool * func * located_expr
  | Def of
      bool * func * located_pattern list * located_expr option * located_expr * with_block
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
  | PInt -> "Int"
  | PFloat -> "Float"
  | PString -> "String"
  | PChar -> "Char"
  | PBool -> "Bool"
  | PAtom -> "Atom"
  | PUnit -> "Unit"
  | PUni ix -> Printf.sprintf "Type %i" ix
;;

let rec show_pat = function
  | _, PWild -> "_"
  | _, PCons (l, r) -> Printf.sprintf "%s :: %s" (show_pat l) (show_pat r)
  | _, PCtor (n, p) -> Printf.sprintf "%s %s" n (show_pat p)
  | _, PList ls -> Printf.sprintf "[ %s ]" (List.map show_pat ls |> String.concat "; ")
  | _, PConst (_, c) ->
    (match c with
     | Ident i -> i
     | Int i -> string_of_int i
     | Float f -> string_of_float f
     | Char c -> Char.escaped c
     | String s -> s
     | Bool b -> string_of_bool b
     | Atom a -> a
     | Unit -> "()")
;;

(* Pretty printing *)
let pp_ident out (i : ident) = Format.fprintf out "%s" i

let pp_prim out (t : prim) =
  Format.fprintf out "%s" (show_prim t)
;;

let pp_const out ((_, c) : located_const) =
  match c with
  | Int i -> Format.fprintf out "%d" i
  | Float f -> Format.fprintf out "%.3f" f
  | String s -> Format.fprintf out "\"%s\"" s
  | Char c -> Format.fprintf out "'%s'" (Char.escaped c)
  | Bool b -> Format.fprintf out "%b" b
  | Atom a -> Format.fprintf out "%@%s" a
  | Unit -> Format.fprintf out "()"
  | Ident i -> pp_ident out i
;;

let pp_binop out (b : binop) =
  let op =
    match b with
    | IAdd -> "+"
    | FAdd -> "+."
    | IMul -> "*"
    | FMul -> "*."
    | ISub -> "-"
    | FSub -> "-."
    | IDiv -> "/"
    | FDiv -> "/."
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

let rec pp_pattern out ((_, arg) : located_pattern) =
  match arg with
  | PConst c -> pp_const out c
  | PWild -> Format.fprintf out "_"
  | PList ps ->
    Format.fprintf
      out
      "[@[<hov>%a@]]"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_pattern)
      ps
  | PCons (l, r) -> Format.fprintf out "(:: @[<hov>%a %a@])" pp_pattern l pp_pattern r
  | PCtor (i, v) -> Format.fprintf out "(%s %a)" i pp_pattern v
;;

let rec pp_expr out ((_, e) : located_expr) =
  match e with
  | Const c -> pp_const out c
  | List l ->
    Format.fprintf
      out
      "[@[<hov>%a@]]"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_expr)
      l
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
  | If (cond, tbranch, fbranch) ->
    Format.fprintf
      out
      "(if@[<v> %a@,%a@,%a@])"
      pp_expr
      cond
      pp_expr
      tbranch
      Format.(pp_print_option ~none:(fun out () -> fprintf out "<none>") pp_expr)
      fbranch
  | Lam (args, body) ->
    Format.fprintf
      out
      "(la@[<v>m (%a) %a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ") pp_pattern)
      args
      pp_expr
      body
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
  | Pi (l, r) -> Format.fprintf out"(%a -> %a)" pp_expr l pp_expr r
  | Binding (i, e) -> Format.fprintf out "(%s : %a)" i pp_expr e
  | TypeLit p ->
    Format.fprintf out "(%a)" pp_prim p
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

let rec pp_ty_decl out ((_, (i, t)) : located_ty_decl) =
  match t with
  | Alias _ ->
    Format.fprintf
      out
      "(ty@[<v>pe %s %a@])"
      i
      pp_tdecl_type
      t (* it looks nicer on a single line *)
  | _ -> Format.fprintf out "(ty@[<v>pe %s@,%a@])" i pp_tdecl_type t

and pp_tdecl_type out (t : tdecl_type) =
  let pp_field out ((i, t) : ident * located_expr) =
    Format.fprintf out "(%s %a)" i pp_expr t
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
  | Dec (_, f, ts) -> Format.fprintf out "(dec %s @[<hov>%a@])" f pp_expr ts
  | Def (_, f, args, when_block, body, with_block) ->
    Format.fprintf
      out
      "(de@[<v>f %s (%a)@,%a@,%a@,%a@])"
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
