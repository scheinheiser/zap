open! Language
open! Parser
open! Rename

let border () =
  Seq.init 30 (fun _ -> '-') |> String.of_seq |> print_endline;
  print_newline ()
;;

let () =
  let input = In_channel.(open_text "examples/typedecls.zap" |> input_all) in
  (* let input = "{a : Type} -> {b : Type} -> (a -> b) -> List a -> List b" in *)
  let l = Lexer.of_string input in
  let res = Parser.parse_program l in
  match res with
  | Ok res ->
    let res = Elab.desugar_program res in
    (* let res = AR.rename_program res in *)
    border ();
    Format.fprintf Format.std_formatter "%a@." Elab.pp_program res;
    (* (match (Typecheck.infer (Typecheck.empty ()) res) with *)
    (* | Error e -> print_endline (Base.Error.to_string_hum e) *)
    (* | Ok (r, _) -> Typed_ast.pp_typed_expr Format.std_formatter r) *)
  | Error e -> print_endline (Base.Error.to_string_hum e)
;;
