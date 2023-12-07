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

let lifeform ?(logpoll = false) ?(print = false) ?(printer = None) (fs : Engine.Fo.form list) =
  let open Utils in
  let nb_correct = ref 0 in
  fs |> List.iter begin fun f ->
    Printf.printf "Formula: %s\n" (Engine.Fo.Notation.f_toascii f);
    flush stdout;
    let g = Flower.of_form f in
    let l = Flower.lifedeath ~logpoll ~printer g in
    if List.is_empty l then incr nb_correct;
    if print = List.is_empty l then begin
      Printf.printf "Original: %s\n" (Flower.string_of_garden g);
      Printf.printf "%s\n" (Flower.(g |> garden_to_gtree |> vehicle |> string_of_vehicle));
      Printf.printf "Reduced : %s\n" (Flower.string_of_garden l);
      Printf.printf "%s\n" (Flower.(l |> garden_to_gtree |> vehicle |> string_of_vehicle));
      print_newline ()
    end
  end;
  let nb_total = List.length fs in
  Printf.printf
    "%n/%n (%.2f%%) correct flowers"
    !nb_correct
    nb_total
    ((float_of_int !nb_correct) /. (float_of_int nb_total) *. 100.)

let test_life ?(print = false) ?(printer = None) garden =
  let open Utils in
  let nb_correct = ref 0 in
  garden |> List.iter begin fun f ->
    (* let l = Flower.life ~printer [f] in *)
    let l = Flower.lifedeath ~printer [f] in
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