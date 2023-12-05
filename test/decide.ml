open Prover

let () =
  Printexc.record_backtrace true;
  Tests.parse_tautos BatIO.stdin |>
  List.map begin fun form ->
    form, Flower.of_form form
  end |>
  List.iter begin fun (form, g) ->
    (* if not (Flower.check g) then begin *)
    Printf.printf "%s\n\n" (Engine.Fo.Notation.f_toascii form);
    Printf.printf "%s\n\n" (Flower.string_of_garden g);
    (* end; *)
    (* Printf.printf "%s\n\n" Flower.(life g |> string_of_garden); *)

    Tests.test_life g;
    (* Tests.test_life ~printer:(Some Flower.string_of_node) g; *)

    (* Printf.printf "%s\n\n" Flower.(g |> garden_to_gtree |> string_of_gtree);
    Printf.printf "%s\n\n" Flower.(g |> garden_to_gtree |> gtree_to_garden |> string_of_garden); *)
    Printf.printf "\n\n";
  end