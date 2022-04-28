open Prover

let parse_tautos ic =
  BatIO.lines_of ic |>
  BatEnum.filter begin fun l ->
    let l = BatString.trim l in
    let starts_with c s = BatString.get s 0 = c in
    (not (BatString.is_empty l)) && not (starts_with '#' l)
  end |>
  BatList.of_enum |>
  List.map begin fun tauto ->
    let open Engine in
    let expr = Io.parse_goal (Io.from_string tauto) in
    let _, _, form = Fo.Goal.check expr in
    form
  end

let () =
  Printexc.record_backtrace true;
  parse_tautos BatIO.stdin |>
  List.map begin fun form ->
    form, Flower.of_form form
  end |>
  List.iter begin fun (form, g) ->
    (* if not (Flower.check g) then begin *)
      Printf.printf "%s\n\n" (Engine.Fo.Notation.f_toascii form);
      Printf.printf "%s\n\n" Flower.(g |> string_of_garden);
    (* end; *)
    (* Printf.printf "%s\n\n" Flower.(life g |> string_of_garden); *)
    Tests.test_life g;
    (* Printf.printf "%s\n\n" Flower.(g |> garden_to_gtree |> string_of_gtree);
    Printf.printf "%s\n\n" Flower.(g |> garden_to_gtree |> gtree_to_garden |> string_of_garden); *)
    Printf.printf "\n\n";
  end