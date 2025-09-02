open Util

(* made using https://github.com/contificate/summary as a reference *)

(* utils *)
let rec any (l : 'a list) (p : 'a -> bool) : bool =
  match l with
  | [] -> false
  | x :: xs -> p x || any xs p
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

  let match_separated_list (tokens : t) (sep : Token.token) (p : t -> 'a) : 'a list =
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

  let match_list (tokens : t) (fin: Token.token) (p : t -> 'a) : 'a list =
    let rec aux acc =
      let v = p tokens in
      if not (matches tokens fin)
      then (
        let _ = advance tokens in
        v :: acc |> aux)
      else v :: acc
    in
    List.rev @@ aux []
  ;;

  let match_with_either
        (tokens : t)
        (p1 : t -> 'a option)
        (p2 : t -> 'a option)
        (msg : string)
    : 'a
    =
    let prev = tokens.tokens in
    match p1 tokens with
    | Some v -> v
    | None ->
      tokens.tokens <- prev;
      (match p2 tokens with
       | Some v -> v
       | None -> Error.report_err (None, msg))
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

  (* optionally consume a token. has no effect if there isn't a match. *)
  let consume_opt (tokens : t) (tok : Token.token) =
    let previous = tokens.tokens in
    let _, next = advance tokens in
    if next <> tok then tokens.tokens <- previous
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
    | LPAREN | RPAREN -> 0
    | NE | EQ -> 2
    | LT | GT | LTE | GTE -> 3
    | PLUS | MINUS -> 4
    | MUL | DIV -> 5
    | NOT -> 6
    | CONS -> 7
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

  let parse_prim (l : Lexer.t) : Ast.prim =
    Lexer.consume_with
      l
      (function
        | TY_PRIM p -> Some p
        | _ -> None)
      "Expected a primitive type."
  ;;

  let rec parse_ty (l : Lexer.t) : Ast.ty =
    match Lexer.advance l with
    | _, TY_PRIM t -> parse_arrow l (Ast.Prim t)
    | _, LBRACK ->
      let ty = parse_ty l in
      Lexer.consume l RBRACK "Expected ']' to end list.";
      parse_arrow l (Ast.List ty)
    | _, LPAREN ->
      let contents = Lexer.match_separated_list l COMMA parse_ty in
      Lexer.consume l RPAREN "Expected ')' to end tuple.";
      parse_arrow l (Ast.Tuple contents)
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf "Unexpected token while parsing type: %s" (Token.show tok) )
  and parse_arrow (l: Lexer.t) (left: Ast.ty) =
    match snd (Lexer.peek l) with
     | ARROW ->
       Lexer.skip l ~am:1;
       let next = parse_ty l in
       Ast.Arrow (left, next)
     | _ -> left
  ;;

  let parse_const (l : Lexer.t) : Ast.expr =
  Lexer.consume_with
    l
    (function
      | INT i -> Some (Ast.Int i)
      | FLOAT f -> Some (Ast.Float f)
      | CHAR c -> Some (Ast.Char c)
      | STRING s -> Some (Ast.String s)
      | BOOL b -> Some (Ast.Bool b)
      | UNIT -> Some Ast.Unit
      | ATSIGN -> Some (Ast.Atom (parse_ident l))
      | _ -> None)
    "Expected a constant."
  ;;

  (* https://www.youtube.com/watch?v=2l1Si4gSb9A *)
  let rec parse_expr (l: Lexer.t) (limit: int) : Ast.expr =
    let left = nud l in
    let lbp = Lexer.current l |> snd |> get_bp in
    let rec go lf =
      if lbp > limit && snd (Lexer.current l) <> EOF && snd (Lexer.current l) <> RPAREN
      then
        let _, op_tok = Lexer.advance l in
        go (led l lf op_tok)
      else
        lf
    in
    go left
  and nud (l: Lexer.t) : Ast.expr =
    match Lexer.advance l with
    | _, INT i -> (Ast.Int i)
    | _, FLOAT f -> (Ast.Float f)
    | _, CHAR c -> (Ast.Char c)
    | _, STRING s -> (Ast.String s)
    | _, BOOL b -> (Ast.Bool b)
    | _, UNIT -> Ast.Unit
    | _, ATSIGN -> (Ast.Atom (parse_ident l))
    | _, IDENT i -> Ast.Ident i
    | _, LBRACK ->
  (* match_separated_list (tokens : t) (sep : Token.token) (p : t -> 'a) : 'a list = *)
      let es = Lexer.match_separated_list l SEMI (Fun.flip parse_expr 0) in
      Lexer.consume l RBRACK "Expected ']' to end list.";
      Ast.EList es
    | _, LPAREN ->
      let e = parse_expr l 0 in
      Lexer.consume l RPAREN "Expected ')' to end grouped expression.";
      e
    | pos, tok -> Error.report_err (Some pos, Printf.sprintf "Unexpected token: %s" (Token.show tok))
  and led (l: Lexer.t) (left: Ast.expr) = function
    | PLUS -> Ast.Bop (left, "+", (parse_expr l 4))
    | MINUS -> Ast.Bop (left, "-", (parse_expr l 4))
    | MUL -> Ast.Bop (left, "*", (parse_expr l 5))
    | DIV -> Ast.Bop (left, "/", (parse_expr l 5))
    | CONS -> Ast.Bop (left, "::", (parse_expr l 7))
    | NE -> Ast.Bop (left, "/=", (parse_expr l 2))
    | EQ -> Ast.Bop (left, "=", (parse_expr l 2))
    | LT -> Ast.Bop (left, "<", (parse_expr l 3))
    | LTE -> Ast.Bop (left, "<=", (parse_expr l 3))
    | GT -> Ast.Bop (left, ">", (parse_expr l 3))
    | GTE -> Ast.Bop (left, ">=", (parse_expr l 3))
    | op -> Error.report_err (None, Printf.sprintf "Expecting binary operator, got '%s'." (Token.show op))
  ;;

  let parse_dec (l : Lexer.t): Ast.definition =
    Lexer.consume l DEC "Expected 'dec' keyword.";
    let n = parse_ident l in
    Lexer.consume l COLON "Expected ':' after 'dec' keyword.";
    let ts = Lexer.match_list l DOT parse_ty in
    Lexer.consume l DOT "Expected '.' after 'dec' sig.";
    Ast.Dec (n, ts)
  ;;

  let parse_module (l : Lexer.t) : Ast.module_name =
    Lexer.consume l ATSIGN "Expected an '@' before 'module' keyword.";
    Lexer.consume l MODULE "Expected the 'module' keyword.";
    parse_upper_ident l
  ;;

  let parse_import_cond (l : Lexer.t) : Ast.import_cond =
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
        (*TODO: change this to work with uppercase idents too *)
        let contents = Lexer.match_separated_list l COMMA parse_ident in
        Lexer.consume l RPAREN "Expected a ')' to end the import condition.";
        contents
      | _, IDENT i -> [ i ]
      | _, UPPER_IDENT i -> [ i ]
      | pos, _ ->
        Error.report_err
          (Some pos, "Expected identifier or comma-separated list of identifiers.")
    in
    if cond_type then Ast.CWith values else Ast.CWithout values
  ;;

  let parse_import (l : Lexer.t) : Ast.import =
    Lexer.consume l ATSIGN "Expected an '@' before 'module' keyword.";
    Lexer.consume l IMPORT "Expected the 'import' keyword.";
    let name = parse_upper_ident l in
    let cond =
      if Lexer.matches l WITH || Lexer.matches l WITHOUT
      then Some (parse_import_cond l)
      else None
    in
    name, cond
  ;;
end
