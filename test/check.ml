open Prover
open Utils

let () =
  Printexc.record_backtrace true;
  let tautos = Tests.parse_tautos BatIO.stdin in
  Printf.printf "%s\n\n" (List.to_string ~sep:"\n" ~left:"" ~right:"" Engine.Fo.Notation.f_toascii tautos);
  Tests.test_life ~printer:(Some Flower.string_of_node) (List.concat_map Flower.of_form tautos)
  (* Tests.test_life (List.concat_map Flower.of_form tautos) *)