open! Language
open! Parser
open! Rename

let border () =
  Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline;
  print_newline ()
;;

let () =
  let input = In_channel.(open_text "examples/dependent.zap" |> input_all) in
  let l = Lexer.of_string input in
  let res' = Parser.parse_program l in
  Ast.pp_program Format.std_formatter res';
  border ();
  let res = Alpha.rename_program res' in
  Ast.pp_program Format.std_formatter res;
;;
