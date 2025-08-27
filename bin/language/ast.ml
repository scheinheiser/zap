type ident = string
type variable = string
type func = string
type module_name = string
type comment = string
type generic = string
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
  | Prefix of func * func_arg list
  | Infix of func_arg * func * func_arg

and func_arg =
  | ALit of const
  | AIdent of ident
  | AAp of func_ap

type list_item =
  | LConst of const
  | LIdent of ident

type term =
  | TLit of const
  | TLet of ident * ty option * term
  | TGrouping of term list
  | TAp of func_ap
  | TIf of term * term * term option
  | TList of list_item list

type import_cond =
  | CWith of ident list
  | CWithout of ident list

type import = module_name * import_cond option

type arg =
  | ArgIdent of ident
  | ArgMatch of match_arg

and match_arg =
  | MWild (* wildcard, '_' *)
  | MLit of const
  | MCons of arg * arg
  | MList (* [] *)

type definition =
  | Dec of func * ty list
  | Def of func * arg list * term option * term list * with_block option
(* identifer, args, optional when-block, body, optional with-block *)

and with_block = definition list

type top_lvl =
  | TDef of definition
  | TImport of import

type program = module_name * top_lvl list
