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

  let parse_const (l : Lexer.t) : Ast.const =
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

  let rec parse_ty (l : Lexer.t) : Ast.ty =
    match Lexer.advance l with
    | _, TY_PRIM t when snd (Lexer.peek l) <> ARROW -> Ast.Prim t
    | _, TY_PRIM t ->
      Lexer.skip l ~am:1;
      let next = parse_ty l in
      Ast.Arrow (Ast.Prim t, next)
    | _, LBRACK ->
      let ty = parse_ty l in
      Lexer.consume l RBRACK "Expected ']' to end list.";
      (match snd (Lexer.peek l) with
       | ARROW ->
         Lexer.skip l ~am:1;
         let next = parse_ty l in
         Ast.Arrow (Ast.List ty, next)
       | _ -> Ast.List ty)
    | _, LPAREN ->
      let contents = Lexer.match_separated_list l COMMA parse_ty in
      Lexer.consume l RPAREN "Expected ')' to end tuple.";
      (match snd (Lexer.peek l) with
       | ARROW ->
         Lexer.skip l ~am:1;
         let next = parse_ty l in
         Ast.Arrow (Ast.Tuple contents, next)
       | _ -> Ast.Tuple contents)
    | pos, tok ->
      Error.report_err
        ( Some pos
        , Printf.sprintf "Unexpected token while parsing type: %s" (Token.show tok) )
  ;;

  (* https://www.youtube.com/watch?v=2l1Si4gSb9A *)
  let parse_infix (l: Lexer.t) : Ast.func_ap =
    let rec parse_infix' lex limit = 
      let left = nud lex in
      Format.fprintf Format.std_formatter "left: %a@." Ast.pp_ap_arg left;
      let lbp = Lexer.current lex |> snd |> get_bp in
      let rec go lf =
        Printf.printf "prec check: %d > %d\n" lbp limit;
        if lbp > limit && snd (Lexer.current l) <> EOF
        then
          let _, op_tok = Lexer.advance l in
          Printf.printf "%s: %d > %d\n" (Token.show op_tok) lbp limit;
          go (led lex lf op_tok)
        else
          lf
      in
      go left
    and nud (l: Lexer.t) : Ast.ap_arg =
      Lexer.match_with_either
        l
        (Fun.compose (fun x -> Some (Ast.AIdent x)) parse_ident)
        (Fun.compose (fun x -> Some (Ast.ALit x)) parse_const)
        "Unexpected token."
    and led (l: Lexer.t) (left: Ast.ap_arg) = function
      | PLUS -> Ast.AAp (Ast.Infix (left, "+", (parse_infix' l 3)))
      | MINUS -> Ast.AAp (Ast.Infix (left, "-", (parse_infix' l 4)))
      | MUL -> Ast.AAp (Ast.Infix (left, "*", (parse_infix' l 5)))
      | DIV -> Ast.AAp (Ast.Infix (left, "/", (parse_infix' l 5)))
      | CONS -> Ast.AAp (Ast.Infix (left, "::", (parse_infix' l 7)))
      | NE -> Ast.AAp (Ast.Infix (left, "/=", (parse_infix' l 2)))
      | EQ -> Ast.AAp (Ast.Infix (left, "=", (parse_infix' l 2)))
      | LT -> Ast.AAp (Ast.Infix (left, "<", (parse_infix' l 3)))
      | LTE -> Ast.AAp (Ast.Infix (left, "<=", (parse_infix' l 3)))
      | GT -> Ast.AAp (Ast.Infix (left, ">", (parse_infix' l 3)))
      | GTE -> Ast.AAp (Ast.Infix (left, ">=", (parse_infix' l 3)))
      | _ as op -> Error.report_err (None, Printf.sprintf "Expecting binary operator, got '%s'." (Token.show op))
    in
    let left = nud l in
    Format.fprintf Format.std_formatter "left: %a@." Ast.pp_ap_arg left;
    let op, p =
      Lexer.consume_with
        l
        (function
          | PLUS as op -> Some ("+", get_bp op)
          | MINUS as op -> Some ("-", get_bp op)
          | MUL as op -> Some ("*", get_bp op)
          | DIV as op -> Some ("/", get_bp op)
          | CONS as op -> Some ("::", get_bp op)
          | NE as op -> Some ("/=", get_bp op)
          | EQ as op -> Some ("=", get_bp op)
          | LT as op -> Some ("<", get_bp op)
          | LTE as op -> Some ("<=", get_bp op)
          | GT as op -> Some (">", get_bp op)
          | GTE as op -> Some (">=", get_bp op)
          | _ -> None)
      "Expecting binary operator."
    in 
    Ast.Infix (left, op, parse_infix' l p)

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
