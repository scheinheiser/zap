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

type const =
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Bool of bool
  | Atom of ident
  | Unit

type func_ap =
  | Prefix of func * ap_arg list
  | Infix of ap_arg * func * ap_arg

and ap_arg =
  | ALit of const
  | AIdent of ident
  | AAp of func_ap

type item =
  | LConst of const
  | LIdent of ident

type term =
  | TLit of const
  | TLet of ident * ty option * term
  | TGrouping of term list
  | TAp of func_ap
  | TIf of term * term * term option
  | TList of item list
  | TTup of item list

type import_cond =
  | CWith of ident list
  | CWithout of ident list

type import = module_name * import_cond option

type def_arg =
  | ArgIdent of ident
  | ArgMatch of match_arg

and match_arg =
  | MWild (* wildcard, '_' *)
  | MLit of const
  | MCons of def_arg * def_arg
  | MList (* [] *)

type definition =
  | Dec of func * ty list
  | Def of func * def_arg list * term option * term list * with_block option
(* identifer, args, optional when-block, body, optional with-block *)

and with_block = definition list

type top_lvl =
  | TDef of definition
  | TImport of import

type program = module_name * top_lvl list

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

let rec pp_func_ap out (ap : func_ap) =
  match ap with
  | Prefix (f, args) ->
    Format.fprintf
      out
      "(@[<hov>%s@ %a@])"
      f
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ") pp_ap_arg)
      args
  | Infix (l, f, r) ->
    Format.fprintf out "(@[<hov>%s@ %a@ %a@])" f pp_ap_arg l pp_ap_arg r

and pp_ap_arg out (arg : ap_arg) =
  match arg with
  | ALit l -> pp_const out l
  | AIdent i -> pp_ident out i
  | AAp ap -> pp_func_ap out ap
;;

let pp_item out (item : item) =
  match item with
  | LConst c -> pp_const out c
  | LIdent i -> pp_ident out i
;;

let rec pp_term out (t : term) =
  match t with
  | TLit c -> pp_const out c
  | TAp ap -> pp_func_ap out ap
  | TList l ->
    Format.fprintf
      out
      "[@[<hov>%a@]]"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_item)
      l
  | TTup t ->
    Format.fprintf
      out
      "(@[<hov>%a@])"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_item)
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
      pp_term
      cond
      pp_term
      tbranch
      Format.(pp_print_option ~none:(fun out () -> fprintf out "<none>") pp_term)
      fbranch
;;

let pp_import_cond out (cond : import_cond) =
  match cond with
  | CWith includes ->
    Format.fprintf
      out
      "with @[<hov>%a@]"
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") pp_ident)
      includes
  | CWithout excludes ->
    Format.fprintf
      out
      "without @[<hov>%a@]"
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

let rec pp_def_arg out (arg : def_arg) =
  match arg with
  | ArgIdent i -> pp_ident out i
  | ArgMatch m_arg -> pp_match_arg out m_arg

and pp_match_arg out (arg : match_arg) =
  match arg with
  | MWild -> Format.fprintf out "_"
  | MList -> Format.fprintf out "[]"
  | MLit lit -> pp_const out lit
  | MCons (l, r) -> Format.fprintf out "(:: @[<hov>%a %a@])" pp_def_arg l pp_def_arg r
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
      Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out " ") pp_def_arg)
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

let pp_top_lvl out (top : top_lvl) =
  match top with
  | TDef def -> pp_definition out def
  | TImport imp -> pp_import out imp
;;

let pp_module out (mod_name : module_name) = Format.fprintf out "(module %s)" mod_name

let pp_program out ((prog_name, prog_body) : program) =
  Format.fprintf
    out
    "%a@.@.%a@."
    pp_module
    prog_name
    Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_top_lvl)
    prog_body
;;
