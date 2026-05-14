open Util

(* Type definitions *)
type binder = int

type ident =
  | Str of string
  | GStr of string * int

type located_const = Location.t * const

and const =
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool
  | Unit
  | Ident of ident
  | AccessIdent of ident list
  | Udc of ident (* user defined costructor *)

type prim =
  | PInt
  | PFloat
  | PString
  | PChar
  | PBool
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

type located_import = Location.t * import
and import = ident * import_cond option

and import_cond =
  | CWith of ident list
  | CWithout of ident list

(* utils *)
let show_prim = function
  | PInt -> "Int"
  | PFloat -> "Float"
  | PString -> "String"
  | PChar -> "Char"
  | PBool -> "Bool"
  | PUnit -> "Unit"
  | PUni ix -> Printf.sprintf "U%i" ix
;;

let get_str = function
  | Str i -> i
  | GStr (i, _) -> i
;;

let get_str_combine = function
  | Str i -> i
  | GStr (i, n) -> i ^ string_of_int n
;;

(* Pretty printing *)
let pp_ident out (i : ident) =
  match i with
  | Str i -> Format.fprintf out "%s" i
  | GStr (i, n) -> Format.fprintf out "%s%i" i n
;;

let pp_prim out (t : prim) = Format.fprintf out "%s" (show_prim t)

let pp_const out ((_, c) : located_const) =
  match c with
  | Int i -> Format.fprintf out "%d" i
  | Float f -> Format.fprintf out "%.3f" f
  | String s -> Format.fprintf out "\"%s\"" s
  | Char c -> Format.fprintf out "'%s'" (Char.escaped c)
  | Bool b -> Format.fprintf out "%b" b
  | Unit -> Format.fprintf out "()"
  | Ident i | Udc i -> pp_ident out i
  | AccessIdent is ->
    let i = List.map get_str is |> String.concat "." in
    Format.fprintf out "%s" i
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
    | User_op o -> Format.asprintf "%a" pp_ident o
  in
  Format.fprintf out "%s" op
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
