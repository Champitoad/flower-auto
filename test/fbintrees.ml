open Prover
open Itree

type btree =
  | Leaf
  | Node of btree * btree

type tree = (unit, unit) Itree.t

let btree_to_itree (t : btree) : tree =
  let rec build index = function
    | Leaf ->
        { parent = None; index; children = [||]; data = Itree.Leaf ((), ()) }
    | Node (n1, n2) ->
        let children = [| build 0 n1; build 1 n2 |] in
        { parent = None; index; children; data = Itree.Node (()) }
  in
  let t = build 0 t in
  link t;
  t

let itree_to_btree t =
  let rec aux t =
    match t.data with
    | Itree.Leaf _ ->
        Leaf
    | Itree.Node _ ->
        Node (aux t.children.(0), aux t.children.(1))
  in aux t

let () =
  let print _ = "*" in
  let to_string = to_string print print in

  let bt = Node (Node (Leaf, Leaf), Node (Leaf, Leaf)) in
  let t = btree_to_itree bt in

  Printf.printf "%s\n" (t |> to_string);
  Printf.printf "%s\n" (t |> itree_to_btree |> btree_to_itree |> to_string);
  