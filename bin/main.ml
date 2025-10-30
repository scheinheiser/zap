open! Language
open! Parser
open! Rename
open! Anf

let border () =
  Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline;
  print_newline ()
;;

let () =
  (* let input = In_channel.(open_text "examples/test.zap" |> input_all) in *)
  let input = "(let _ := 10 in 10 + 2)" in
  print_endline input;
  border ();
  let l = Lexer.of_string input in
  let res' = Parser.parse_expr l 0 (fresh_om ()) in
  Ast.pp_expr Format.std_formatter res'
  (* let res = Alpha.rename_program res' in *)
  (* Ast.pp_program Format.std_formatter res; *)
  (* border (); *)
  (* match Typecheck.check_program res with *)
  (* | Ok p -> *)
  (*   (* ANF.of_typed_program p |> fun p -> ANF.pp_program Format.std_formatter p *) *)
  (*   Typed_ast.pp_typed_program Format.std_formatter p *)
  (* | Error e -> print_endline (Base.Error.to_string_hum e) *)
;;
