open Prover.Tests

let () =
  Random.self_init ();
  let g = test_garden ~correct:false 10000 { depth = 3; max_petals = 2; max_flowers = 4 } in
  test_life g;