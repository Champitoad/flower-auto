open Utils

type ('a, 'b) data =
  | Leaf of 'a * 'b
  | Node of 'a

type ('a, 'b) t =
  { mutable parent : ('a, 'b) t option;
    mutable index : int;
    children : ('a, 'b) t BatDynArray.t;
    mutable data : ('a, 'b) data; }

let rec to_string print_leaf print_node t =
  let data =
    match t.data with
    | Leaf (a, b) ->
        Printf.sprintf "%s : %s"
          (print_leaf b)
          (print_node a)
    | Node a ->
        print_node a in
  let children =
    t.children |>
    BatDynArray.map (to_string print_leaf print_node) |>
    BatDynArray.to_list |>
    String.concat ", " in
  Printf.sprintf "%s (%s)" data children

let leaf_data t =
  match t.data with
  | Leaf (_, data) -> data
  | _ -> failwith "Not a leaf"

let node_data t =
  match t.data with
  | Leaf (data, _) | Node (data) -> data

let link t =
  let rec aux parent i t =
    t.parent <- parent;
    t.index <- i;
    BatDynArray.iteri (aux (Some t)) t.children
  in
  aux t.parent t.index t

let parent t =
  t.parent

type path = int list

let string_of_path =
  Utils.List.to_string string_of_int

let is_subpath p p' =
  Engine.Utils.List.is_prefix p' p

let root_path ?(stop = None) t =
  let rec aux acc t =
    match t.parent, stop with
    | Some p, None ->
        aux (t.index :: acc) p
    | Some p, Some p' when p != p' ->
        aux (t.index :: acc) p
    | _ ->
        t, acc
  in aux [] t

let root t =
  fst (root_path t)

let path ?(stop = None) t =
  snd (root_path ~stop t)

exception InvalidPath

let child t n =
  try
    BatDynArray.get t.children n
  with _ -> raise InvalidPath

let insert_child t i c =
  BatDynArray.insert t.children i c;
  c.parent <- Some t;
  BatDynArray.iteri begin fun j c ->
    if j > i then begin
      c.index <- c.index + 1;
    end
  end t.children

let remove_child t i =
  (BatDynArray.get t.children i).parent <- None;
  BatDynArray.delete t.children i;
  BatDynArray.iteri begin fun j c ->
    if j >= i then begin
      c.index <- c.index - 1;
    end
  end t.children

let desc t p =
  List.fold_left child t p

let is_desc t t' =
  is_subpath (path t) (path t')

let in_same_tree t t' =
  root t == root t'

(** Least common ancestor *)
let lca t1 t2 =
  let r, p1 = root_path t1 in
  let p2 = path t2 in
  let gcp = List.gcp p1 p2 in
  desc r gcp

(** Deep equality and copy *)

let eq ?(eq_data = (=)) t t' =
  let rec aux t t' =
    t.index = t'.index &&
    eq_data t.data t'.data &&
    BatDynArray.length t.children = BatDynArray.length t'.children &&
    BatDynArray.for_all2 aux t.children t'.children in
  let r = root t in
  let r' = root t' in
  aux r r'

let deepcopy ?(copy_data = identity) t =
  let rec aux t =
    let index = t.index in
    let data = copy_data t.data in
    let children = BatDynArray.map aux t.children in
    let t = { index; data; parent = None; children } in
    link t; t in
  let r = root t in
  desc (aux r) (path t)