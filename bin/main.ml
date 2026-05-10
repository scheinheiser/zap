open! Language
open! Parser

let border () =
  Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline;
  print_newline ()
;;

let () =
  (* let input = In_channel.(open_text "examples/dependent.zap" |> input_all) in *)
  let input = "(x: Int) -> x" in
  let l = Lexer.of_string input in
  let res = Parser.parse_expr l 0 (fresh_om ()) in
  Format.fprintf Format.std_formatter "%a@." Ast.pp_expr res;
  border ();
  (* let res = AR.rename_program res in *)
  match (Typecheck.infer (Typecheck.empty ()) res) with
  | Error e -> print_endline (Base.Error.to_string_hum e)
  | Ok (r, _) -> Typed_ast.pp_typed_expr Format.std_formatter r
