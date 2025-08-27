open Lexing
open Language

let column pos = pos.pos_cnum - pos.pos_bol - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int (column pos + 1) in
  "line " ^ l ^ ", column " ^ c
;;

let parse' f s =
  let lexbuf = Lexing.from_string s in
  try f Lexer.tokenize lexbuf with
  | Parser.Error -> raise (Failure ("Parse error at " ^ pos_string lexbuf.lex_curr_p))
;;

let parse_program s = parse' Parser.program s

let () =
  let _ =
    "@module Syntax\n\
     dec func : int -> int.\n\
     def func num :=\n\
    \  let inc := num + 1 in\n\
    \  num\n\
     ;;"
  in
  let f = In_channel.(open_text "test.zap" |> input_all) in
  print_endline f;
  let _ = parse_program f in
  print_endline "success"
;;
