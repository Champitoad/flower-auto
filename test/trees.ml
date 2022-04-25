open Prover
open Utils
open Itree

let t1 : (unit, string) t =
  let rec r = { parent = None; index = 0; children = BatDynArray.of_list [ n0; n1 ]; data = Node (()) }
  and n0 = { parent = Some r; index = 0; children = BatDynArray.of_list [ a; b ]; data = Node (()) }
  and n1 = { parent = Some r; index = 1; children = BatDynArray.of_list [ c ]; data = Node (()) }
  and a = { parent = Some n0; index = 0; children = BatDynArray.of_list []; data = Leaf ((), "a") }
  and b = { parent = Some n0; index = 1; children = BatDynArray.of_list []; data = Leaf ((), "b") }
  and c = { parent = Some n1; index = 1; children = BatDynArray.of_list []; data = Leaf ((), "c") }
  in r

let to_string =
  Itree.to_string identity (fun _ -> "")

let print t =
  Printf.printf "%s\n" (to_string t)

let print_option t =
  Printf.printf "%s\n" (Option.to_string to_string t)

let print_ref_option t =
  Printf.printf "%s\n" (Option.to_string (fun t -> to_string !t) t)

let print_path p =
  Printf.printf "%s\n" (List.to_string string_of_int p)

let () =
  print t1;
  let t100 = desc t1 [0; 0] in
  let t101 = desc t1 [0; 1] in
  print t100;
  print t101;
  let anc = lca t100 t101 in
  print_option anc;