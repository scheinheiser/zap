open! Language
open! Parser

let border () =
  Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline;
  print_newline ()
;;

let () =
  let input = In_channel.(open_text "examples/flpmtest.zap" |> input_all) in
  (* let input = "[]" in *)
  let l = Lexer.of_string input in
  let res = Parser.parse_program l in
  let res = Desugar.desugar_program res in
  Printf.printf "%s\n" input;
  border ();
  Format.fprintf Format.std_formatter "%a@." Desugar.pp_program res;
  (* let res = AR.rename_program res in *)
  (* match (Typecheck.infer (Typecheck.empty ()) res) with *)
  (* | Error e -> print_endline (Base.Error.to_string_hum e) *)
  (* | Ok (r, _) -> Typed_ast.pp_typed_expr Format.std_formatter r *)
