open Prover

let modus_ponens =
  Flower.Flower ([Atom "a"; Flower ([Atom "a"], [[Atom "b"]])], [[Atom "b"]])

let test_garden ?(correct = true) nb_flowers params =
  let open Flower.Random in
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

let test_check ?(loadbar = true) ?(print = false) garden =
  let open Utils in
  garden |> List.iter begin fun f ->
    if print = Flower.check_flower f then
      print_endline (Flower.string_of_flower f)
  end

let () =
  Random.self_init ();
  (* test_assumptions (test_garden ~correct:false 100 { depth = 4; max_petals = 2; max_flowers = 4 }); *)
  (* test_check ~print:true (test_garden 1 { depth = 10; max_petals = 2; max_flowers = 3 }); *)
  test_check (test_garden 100000 { depth = 4; max_petals = 2; max_flowers = 2 });