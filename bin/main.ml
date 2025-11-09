open! Language
open! Parser
open! Rename
open! Anf

let border () =
  Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline;
  print_newline ()
;;

let () =
  let input = In_channel.(open_text "examples/match.zap" |> input_all) in
  let l = Lexer.of_string input in
  let res' = Parser.parse_program l in
  let res = Alpha.rename_program res' in
  Ast.pp_program Format.std_formatter res;
  border ();
  match Typecheck.check_program res with
  | Ok p ->
    (* ANF.of_typed_program p |> fun p -> ANF.pp_program Format.std_formatter p *)
    Typed_ast.pp_typed_program Format.std_formatter p
  | Error e -> Printf.printf "%s\n" (Base.Error.to_string_hum e)
;;
