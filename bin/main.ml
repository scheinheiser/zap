open! Language
open! Parser
open! Newparser
open! Rename
open! Typecheck

let border () =
  Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline;
  print_newline ()
;;

let () =
  (* let input = In_channel.(open_text "examples/dependent.zap" |> input_all) in *)
  let input = "length (show 10)" in
  print_endline input;
  let l = Lexer.of_string input in
  let res = Parser.parse_expr l 0 (fresh_om ()) in
  (*   match res with *)
  (*   | MParser.Success res -> Format.fprintf Format.std_formatter "%a@." Ast.pp_expr res *)
  (*   | MParser.Failed (e, _) -> print_endline e *)
  (* ;; *)
  border ();
  Format.fprintf Format.std_formatter "%a@." Ast.pp_expr res
;;
(* let res = Alpha.rename_program res' in *)
(* match (Typecheck.infer (Typecheck.empty ()) res) with *)
(* | Error e -> print_endline (Base.Error.to_string_hum e) *)
(* | Ok (r, _) -> Typed_ast.pp_typed_expr Format.std_formatter r *)
