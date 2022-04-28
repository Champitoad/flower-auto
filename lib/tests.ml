open Flower

let modus_ponens =
  Flower ([Atom "a"; Flower ([Atom "a"], [[Atom "b"]])], [[Atom "b"]])

let tnd =
  Flower ([Flower ([Flower ([], [[Atom "a"]; [Flower ([Atom "a"], [])]])], [])], [])

let test_garden ?(correct = true) nb_flowers params =
  let open Random in
  let generate = if correct then Correct.gen_flower else gen_flower in
  List.init nb_flowers (fun _ -> reset_freshness (); generate ~params ())

let test_assumptions garden =
  let open Utils in
  garden |> List.iter begin fun f ->
    print_endline (Flower.string_of_flower f);
    print_endline begin
      Flower.assumptions_flower f |>
      Set.to_list |> List.to_string identity
    end;
    print_newline ()
  end

let test_check ?(print = false) garden =
  let open Utils in
  garden |> List.iter begin fun f ->
    if print = Flower.check_flower f then begin
      print_endline (Flower.string_of_flower f);
      print_newline ()
    end
  end

let test_life ?(print = false) garden =
  let open Utils in
  let nb_correct = ref 0 in
  garden |> List.iter begin fun f ->
    let l = Flower.life [f] in
    if List.is_empty l then incr nb_correct;
    if print = List.is_empty l then begin
      Printf.printf "Original: %s\n" (Flower.string_of_flower f);
      Printf.printf "%s\n" (Flower.([f] |> garden_to_gtree |> vehicle |> string_of_vehicle));
      Printf.printf "Reduced : %s\n" (Flower.string_of_garden l);
      Printf.printf "%s\n" (Flower.(l |> garden_to_gtree |> vehicle |> string_of_vehicle));
      print_newline ()
    end
  end;
  let nb_total = List.length garden in
  Printf.printf
    "%n/%n (%.2f%%) correct flowers"
    !nb_correct
    nb_total
    ((float_of_int !nb_correct) /. (float_of_int nb_total) *. 100.)