open Util
(* made using https://github.com/contificate/summary as a reference *)

(* utils *)
let rec any (l : 'a list) (p : 'a -> bool) : bool =
  match l with
  | [] -> false
  | x :: xs -> p x || any xs p
;;

let is_const (t : Token.token) : bool =
  match t with
  | INT _ | FLOAT _ | STRING _ | CHAR _ | BOOL _ | ATSIGN | UNIT -> true
  | _ -> false
;;

let is_op (t : Token.token) : bool =
  match t with
  | OP _ | PLUS | MINUS | DIV | MUL | AND | OR | LT | GT | LTE | GTE | CONS | NE | EQ ->
    true
  | _ -> false
;;

let op_to_string (t : Token.token) : string =
  match t with
  | OP o -> o
  | PLUS -> "+"
  | MINUS -> "-"
  | DIV -> "/"
  | MUL -> "*"
  | AND -> "&&"
  | OR -> "||"
  | LT -> "<"
  | GT -> ">"
  | LTE -> "<="
  | GTE -> ">="
  | CONS -> "::"
  | NE -> "/="
  | EQ -> "="
  | _ -> ""
;;

let is_delim (t : Token.token) : bool =
  match t with
  | EOF | RPAREN | COMMA | RBRACE | IN | SEMI | SEMISEMI | IF | THEN | ELSE -> true
  | _ -> false
;;

(* main things *)
module Lexer = struct
  type t = { mutable tokens : Token.t list }

  let of_lexbuf (lexbuf : Lexing.lexbuf) : t =
    let rec aux acc =
      let next = Lexer.token lexbuf in
      match next with
      | _, Token.EOF ->
        let res = List.rev (next :: acc) in
        { tokens = res }
      | _ -> aux (next :: acc)
    in
    aux []
  ;;

  let of_string (s : string) : t = Lexing.from_string s |> of_lexbuf
  let current ({ tokens } : t) : Token.t = List.hd tokens

  let advance (stream : t) : Token.t =
    match stream.tokens with
    | t :: ts ->
      stream.tokens <- ts;
      t
    | [] -> Location.dummy_loc, Token.EOF
  ;;

  let peek (tokens : t) : Token.t =
    let previous = tokens.tokens in
    let next = advance tokens in
    tokens.tokens <- previous;
    next
  ;;

  let skip (tokens : t) ~am:(n : int) =
    if n < 0
    then
      raise
        (Failure
           "Internal error: called Language.Parser.Lexer.skip with a negative amount.")
    else (
      let rec aux t =
        if t <> 0
        then (
          let _ = advance tokens in
          aux (t - 1))
      in
      aux n)
  ;;

  let matches (tokens : t) (tok : Token.token) : bool =
    let _, next = peek tokens in
    next = tok
  ;;

  let separated_list (tokens : t) (sep : Token.token) (p : t -> 'a) : 'a list =
    let rec aux acc =
      let v = p tokens in
      if matches tokens sep
      then (
        let _ = advance tokens in
        v :: acc |> aux)
      else v :: acc
    in
    List.rev @@ aux []
  ;;

  let list_with_end (stream : t) (is_end : Token.token -> bool) (p : t -> 'a) : 'a list =
    let rec go l acc =
      let _, next = peek l in
      match next with
      | t when is_end t -> List.rev acc
      | _ -> go l (p l :: acc)
    in
    go stream []
  ;;

  let consume (tokens : t) (tok : Token.token) (msg : string) =
    let pos, next = advance tokens in
    if next <> tok then Error.report_err (Some pos, msg)
  ;;

  let consume_with (tokens : t) (f : Token.token -> 'a option) (msg : string) : 'a =
    let pos, next = advance tokens in
    match f next with
    | Some v -> v
    | None -> Error.report_err (Some pos, msg)
  ;;

  let consume_any_of (tokens : t) (tok : Token.token list) (msg : string) =
    let pos, next = advance tokens in
    if not @@ any tok (( = ) next) then Error.report_err (Some pos, msg)
  ;;
end

module Parser = struct
  open Token

  let get_bp (t : Token.token) : int =
    match t with
    | LPAREN | RPAREN ->
      1 (* we give this a slight bit of precedence for cases like `length (show 10)` *)
    | NE | EQ -> 2
    | AND | OR -> 3
    | LT | GT | LTE | GTE -> 4
    | PLUS | MINUS -> 5
    | MUL | DIV -> 6
    | NOT -> 7
    | CONS -> 8
    | OP _ -> 9
    | IDENT _ | INT _ | FLOAT _ | CHAR _ | STRING _ | BOOL _ | UNIT | ATSIGN -> 10
    | EOF -> -1
    | _ -> 0
  ;;

  let parse_upper_ident (l : Lexer.t) : Ast.ident =
    Lexer.consume_with
      l
      (function
        | UPPER_IDENT i -> Some i
        | _ -> None)
      "Expected a capitalised identifier."
  ;;

  let parse_ident (l : Lexer.t) : Ast.ident =
    Lexer.consume_with
      l
      (function
        | IDENT i -> Some i
        | _ -> None)
      "Expected an identifier."
  ;;

  let rec parse_ty (l : Lexer.t) : Ast.ty =
    match Lexer.advance l with
    | _, TY_PRIM t -> parse_arrow l (Ast.Prim t)
    | _, IDENT i -> parse_arrow l (Ast.Udt i)
    | _, LBRACK ->
      let ty = parse_ty l in
      Lexer.consume l RBRACK "Expected ']' to end list.";
      parse_arrow l (Ast.List ty)
    | _, LPAREN ->
      let contents = Lexer.separated_list l COMMA parse_ty in
      Lexer.consume l RPAREN "Expected ')' to end tuple.";
      parse_arrow l (Ast.Tuple contents)
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf "Unexpected token while parsing type: %s" (Token.show tok) )

  and parse_arrow (l : Lexer.t) (left : Ast.ty) =
    match snd (Lexer.peek l) with
    | ARROW ->
      Lexer.skip l ~am:1;
      let next = parse_ty l in
      Ast.Arrow (left, next)
    | _ -> left
  ;;

  (* https://www.youtube.com/watch?v=2l1Si4gSb9A *)
  let rec parse_expr (l : Lexer.t) (limit : int) : Ast.expr =
    let left = nud l in
    let lbp = Lexer.current l |> snd |> get_bp in
    let rec go lf =
      if lbp > limit && not (Lexer.current l |> snd |> is_delim)
      then (
        let _, op_tok = Lexer.advance l in
        go (led l lf (get_bp op_tok) op_tok))
      else lf
    in
    go left

  and nud (l : Lexer.t) : Ast.expr =
    match Lexer.advance l with
    | _, INT i -> Ast.Const (Ast.Int i)
    | _, FLOAT f -> Ast.Const (Ast.Float f)
    | _, CHAR c -> Ast.Const (Ast.Char c)
    | _, STRING s -> Ast.Const (Ast.String s)
    | _, BOOL b -> Ast.Const (Ast.Bool b)
    | _, UNIT -> Ast.Const Ast.Unit
    | _, ATSIGN -> Ast.Const (Ast.Atom (parse_ident l))
    | _, IDENT i -> Ast.Ident i
    | _, LBRACK ->
      let es =
        match Lexer.current l |> snd with
        | RBRACK -> []
        | _ -> Lexer.separated_list l SEMI (Fun.flip parse_expr 0)
      in
      Lexer.consume l RBRACK "Expected ']' to end list.";
      Ast.EList es
    | _, LPAREN ->
      let e = parse_expr l 0 in
      Lexer.consume l RPAREN "Expected ')' to end grouped expression or prefix operator.";
      e
    | _, KOP ->
      let pos, next = Lexer.advance l in
      (match next with
       | o when is_op o -> Ast.Ident (op_to_string o)
       | op ->
         Error.report_err
           ( Some pos
           , Printf.sprintf
               "Expected operator to follow 'op' keyword, but got '%s'."
               (Token.show op) ))
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf "Unexpected token while parsing left: %s" (Token.show tok) )

  and led (l : Lexer.t) (left : Ast.expr) (lbp : int) = function
    | PLUS -> Ast.Bop (left, "+", parse_expr l lbp)
    | MINUS -> Ast.Bop (left, "-", parse_expr l lbp)
    | MUL -> Ast.Bop (left, "*", parse_expr l lbp)
    | DIV -> Ast.Bop (left, "/", parse_expr l lbp)
    | CONS -> Ast.Bop (left, "::", parse_expr l lbp)
    | NE -> Ast.Bop (left, "/=", parse_expr l lbp)
    | EQ -> Ast.Bop (left, "=", parse_expr l lbp)
    | LT -> Ast.Bop (left, "<", parse_expr l lbp)
    | LTE -> Ast.Bop (left, "<=", parse_expr l lbp)
    | GT -> Ast.Bop (left, ">", parse_expr l lbp)
    | GTE -> Ast.Bop (left, ">=", parse_expr l lbp)
    | AND -> Ast.Bop (left, "&&", parse_expr l lbp)
    | OR -> Ast.Bop (left, "||", parse_expr l lbp)
    | OP o -> Ast.Bop (left, o, parse_expr l lbp)
    | IDENT i -> Ast.Ap (left, Ast.Ident i)
    | INT i -> Ast.Ap (left, Ast.Const (Ast.Int i))
    | FLOAT f -> Ast.Ap (left, Ast.Const (Ast.Float f))
    | CHAR c -> Ast.Ap (left, Ast.Const (Ast.Char c))
    | STRING s -> Ast.Ap (left, Ast.Const (Ast.String s))
    | BOOL b -> Ast.Ap (left, Ast.Const (Ast.Bool b))
    | UNIT -> Ast.Ap (left, Ast.Const Ast.Unit)
    | ATSIGN -> Ast.Ap (left, Ast.Const (Ast.Atom (parse_ident l)))
    | LBRACK ->
      let es = Lexer.separated_list l SEMI (Fun.flip parse_expr 0) in
      Lexer.consume l RBRACK "Expected ']' to end list.";
      Ast.Ap (left, Ast.EList es)
    | LPAREN ->
      let e = parse_expr l 0 in
      Lexer.consume l RPAREN "Expected ')' to end grouped expression.";
      Ast.Ap (left, e)
    | op ->
      Error.report_err
        (None, Printf.sprintf "Expected binary operator, got '%s'." (Token.show op))
  ;;

  let rec parse_args (l : Lexer.t) : Ast.pattern list =
    Lexer.list_with_end
      l
      (function
        | WILDCARD | LBRACK | LPAREN | IDENT _ -> false
        | t when is_const t -> false
        | _ -> true)
      parse_pattern

  and parse_pattern (l : Lexer.t) : Ast.pattern =
    match Lexer.advance l with
    | _, WILDCARD -> Ast.PWild
    | _, LBRACK ->
      let contents =
        match Lexer.current l |> snd with
        | RBRACK -> []
        | _ -> Lexer.separated_list l SEMI parse_pattern
      in
      Lexer.consume l RBRACK "Expected ']' to end empty list pattern.";
      Ast.PList contents
    | _, LPAREN ->
      let l' = parse_pattern l in
      (match Lexer.current l with
      | _, CONS ->
        Lexer.skip ~am:1 l;
        let r = parse_pattern l in
        Lexer.consume l RPAREN "Expected ')' to end list cons pattern.";
        Ast.PCons (l', r)
      | _, COMMA ->
        Lexer.skip ~am:1 l;
        let rest = Lexer.separated_list l COMMA parse_pattern in
        Lexer.consume l RPAREN "Expected ')' to end tuple pattern.";
        Ast.PTup (l' :: rest)
      | _, RPAREN ->
        Lexer.skip ~am:1 l;
        l'
      | pos, tok -> Error.report_err (Some pos, Printf.sprintf "Expected tuple pattern, cons pattern or a pattern, but got '%s'." (Token.show tok)))
    | _, IDENT i -> Ast.PIdent i
    | _, INT i -> Ast.PConst (Ast.Int i)
    | _, FLOAT i -> Ast.PConst (Ast.Float i)
    | _, STRING i -> Ast.PConst (Ast.String i)
    | _, CHAR i -> Ast.PConst (Ast.Char i)
    | _, BOOL i -> Ast.PConst (Ast.Bool i)
    | _, UNIT -> Ast.PConst Ast.Unit
    | _, ATSIGN -> Ast.PConst (Ast.Atom (parse_ident l))
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf
            "Expected definition argument pattern, but got '%s'."
            (Token.show tok) )
  ;;

  let rec parse_term (l : Lexer.t) : Ast.term =
    match Lexer.current l with
    | _, LET ->
      Lexer.skip ~am:1 l;
      let i = parse_ident l in
      let ty =
        match Lexer.current l with
        | _, COLON ->
          Lexer.skip ~am:1 l;
          let ty' = parse_ty l in
          Lexer.consume l EQ "Expected '=' after type in let-expression.";
          Some ty'
        | _, ASSIGNMENT ->
          Lexer.skip ~am:1 l;
          None
        | pos, tok ->
          Error.report_err
            ( Some pos
            , Printf.sprintf "Unexpected token in let-expression: %s" (Token.show tok) )
      in
      let expr = parse_term l in
      Ast.TLet (i, ty, expr)
    | _, LBRACE ->
      Lexer.skip ~am:1 l;
      let terms = Lexer.separated_list l IN parse_term in
      Lexer.consume l RBRACE "Expected '}' to end grouping.";
      Ast.TGrouping terms
    | _, LPAREN ->
      Lexer.skip ~am:1 l;
      let first = parse_expr l 0 in
      (match Lexer.current l with
       | _, RPAREN ->
         Lexer.skip ~am:1 l;
         Ast.TExpr first
       | _, COMMA ->
         Lexer.skip ~am:1 l;
         let rest = Lexer.separated_list l COMMA (Fun.flip parse_expr 0) in
         Lexer.consume l RPAREN "Expected ')' to end tuple term.";
         Ast.TTup (first :: rest)
       | pos, tok ->
         Error.report_err
           (Some pos, Printf.sprintf "Expected ')' or ',', but got '%s'." (Token.show tok)))
    | _, IF ->
      Lexer.skip ~am:1 l;
      let cond = parse_expr l 0 in
      Lexer.consume l THEN "Expected 'then' keyword after if statement condition.";
      let texpr = parse_term l in
      let fexpr =
        match Lexer.current l with
        | _, ELSE ->
          Lexer.skip ~am:1 l;
          Some (parse_term l)
        | _ -> None
      in
      Ast.TIf (cond, texpr, fexpr)
    | _, LAM ->
      Lexer.skip ~am:1 l;
      let args = parse_args l in
      Lexer.consume l ARROW "Expected '->' after lambda arguments.";
      let body = parse_term l in
      Ast.TLam (args, body)
    | _ -> Ast.TExpr (parse_expr l 0)
  ;;

  let rec parse_definition (l : Lexer.t) : Ast.definition =
    match Lexer.current l with
    | _, DEC -> parse_dec l
    | _, DEF -> parse_def l
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf
            "Expected 'dec' or 'def' keyword to begin a definition, but got '%s'."
            (Token.show tok) )

  and parse_def (l : Lexer.t) : Ast.definition =
    Lexer.consume l DEF "Expected 'def' keyword.";
    let n = parse_ident l in
    let args = parse_args l in
    let when_block =
      match Lexer.current l with
      | _, COLON ->
        Lexer.skip l ~am:1;
        Lexer.consume l WHEN "Expected 'when' after ':' in def.";
        let block = parse_term l in
        Lexer.consume l EQ "Expected '=' after when block.";
        Some block
      | _, ASSIGNMENT ->
        Lexer.skip l ~am:1;
        None
      | pos, tok ->
        Error.report_err
          ( Some pos
          , Printf.sprintf
              "Expected ':' or ':=' after definition arguments, but got '%s'."
              (Token.show tok) )
    in
    let body = Lexer.separated_list l IN parse_term in
    let with_block =
      match Lexer.current l with
      | _, WITH ->
        Lexer.skip l ~am:1;
        let block =
          Lexer.list_with_end l (fun t -> t = SEMI || SEMISEMI = t) parse_definition
        in
        Some block
      | _, SEMISEMI | _, SEMI -> None
      | pos, tok ->
        Error.report_err
          ( Some pos
          , Printf.sprintf
              "Expected either ';;', ';' or with-block to end function definition, but \
               got '%s'."
              (Token.show tok) )
    in
    Lexer.consume_any_of
      l
      [ SEMISEMI; SEMI ]
      "Expected ';;' or ';' to end function definition.";
    Ast.Def (n, args, when_block, body, with_block)

  and parse_dec (l : Lexer.t) : Ast.definition =
    Lexer.consume l DEC "Expected 'dec' keyword.";
    let n = parse_ident l in
    Lexer.consume l COLON "Expected ':' after 'dec' keyword.";
    let ts = Lexer.list_with_end l (fun t -> t = DOT) parse_ty in
    Lexer.consume l DOT "Expected '.' after 'dec' sig.";
    Ast.Dec (n, ts)
  ;;

  let rec parse_tydecl (l : Lexer.t) : Ast.ty_decl =
    Lexer.consume l TYPE "Expected 'type' keyword before type declaration.";
    let ident = parse_ident l in
    Lexer.consume l ASSIGNMENT "Expected ':=' after type identifier.";
    let ty = parse_tydecl_type l in
    ident, ty

  and parse_tydecl_type (l : Lexer.t) : Ast.tdecl_type =
    match Lexer.current l with
    | _, LBRACE ->
      Lexer.skip ~am:1 l;
      let parse_field lex =
        let i = parse_ident lex in
        Lexer.consume l TILDE "Expected '~' after field name.";
        let t = parse_ty lex in
        i, t
      in
      let fields = Lexer.separated_list l SEMI parse_field in
      Lexer.consume l RBRACE "Expected '}' after field declaration in records.";
      Ast.Record fields
    | _, PIPE ->
      Lexer.skip ~am:1 l;
      let parse_variant lex =
        let i = parse_upper_ident lex in
        match Lexer.current lex with
        | _, TILDE ->
          Lexer.skip ~am:1 lex;
          let t = parse_ty lex in
          i, Some t
        | _ -> i, None
      in
      let fields = Lexer.separated_list l PIPE parse_variant in
      Ast.Variant fields
    | _ -> Ast.Alias (parse_ty l)
  ;;

  let parse_module (l : Lexer.t) : Ast.module_name =
    Lexer.consume l ATSIGN "Expected an '@' before 'module' keyword.";
    Lexer.consume l MODULE "Expected the 'module' keyword.";
    parse_upper_ident l
  ;;

  let rec parse_import (l : Lexer.t) : Ast.import =
    Lexer.consume l ATSIGN "Expected an '@' before 'import' keyword.";
    Lexer.consume l IMPORT "Expected the 'import' keyword.";
    let name = parse_upper_ident l in
    let cond =
      if Lexer.matches l WITH || Lexer.matches l WITHOUT
      then Some (parse_import_cond l)
      else None
    in
    name, cond

  and parse_import_cond (l : Lexer.t) : Ast.import_cond =
    let cond_type =
      Lexer.consume_with
        l
        (function
          | WITH -> Some true
          | WITHOUT -> Some false
          | _ -> None)
        "Expected an import condition."
    in
    let values =
      match Lexer.advance l with
      | _, LPAREN ->
        let contents = Lexer.separated_list l COMMA parse_import_ident in
        Lexer.consume l RPAREN "Expected a ')' to end the import condition.";
        contents
      | _, IDENT i -> [ i ]
      | _, UPPER_IDENT i -> [ i ]
      | pos, _ ->
        Error.report_err
          (Some pos, "Expected identifier or comma-separated list of identifiers.")
    in
    if cond_type then Ast.CWith values else Ast.CWithout values

  and parse_import_ident (l : Lexer.t) : Ast.ident =
    Lexer.consume_with
      l
      (function
        | UPPER_IDENT i -> Some i
        | IDENT i -> Some i
        | _ -> None)
      "Expected an identifier."
  ;;

  let parse_toplvl (l : Lexer.t) : Ast.top_lvl =
    match Lexer.current l with
    | _, ATSIGN -> Ast.TImport (parse_import l)
    | _, DEC -> Ast.TDef (parse_dec l)
    | _, DEF -> Ast.TDef (parse_def l)
    | _, TYPE -> Ast.TTyDecl (parse_tydecl l)
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf
            "Expected an import, declaration or definition but got '%s'."
            (Token.show tok) )
  ;;

  let parse_program (l : Lexer.t) : Ast.program =
    let mod' = parse_module l in
    let prog = Lexer.list_with_end l (( = ) EOF) parse_toplvl in
    let imports =
      List.filter_map
        (function
          | Ast.TImport i -> Some i
          | _ -> None)
        prog
    in
    let tydecls =
      List.filter_map
        (function
          | Ast.TTyDecl t -> Some t
          | _ -> None)
        prog
    in
    let body =
      List.filter_map
        (function
          | Ast.TDef d -> Some d
          | _ -> None)
        prog
    in
    mod', imports, tydecls, body
  ;;
end
