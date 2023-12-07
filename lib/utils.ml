let identity = fun x -> x

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

let (|>>) f g = fun x -> g (f x)
let (<<|) f g = fun x -> f (g x)

let rec fixpoint (f : 'a -> 'a) x =
  let y = f x in
  if y = x then x else (fixpoint f) y

module Option = struct
  include BatOption

  let to_string print = function
    | None -> "None"
    | Some x -> print x
end

module List = struct
  include BatList

  let subsetq l l' =
    List.for_all (fun x -> List.memq x l') l

  let same_elements_q l l' =
    subsetq l l' && subsetq l' l

  let diff l l' =
    remove_if (fun a -> List.mem a l') l
  
  (** Greatest Common Prefix *)
  let rec gcp l l' =
    match l, l' with
    | a :: l, a' :: l' when a = a' ->
        a :: gcp l l'
    | _ -> []

  let to_string ?(sep = "; ") ?(left = "[") ?(right = "]") print =
    map print |>> String.concat sep |>> fun s -> left ^ s ^ right
end

module Array = struct
  include BatArray

  let from (i : int) (t : 'a t) : 'a t =
    Array.init (Array.length t - i) (fun j -> t.(i+j))
end

module Set = struct
  include BatSet

  let intersectionf (point : 'a) (f : 'a t t) : 'a t =
    fold intersect f (singleton point)
  
  let unionf (f : 'a t t) : 'a t =
    fold union f empty
  
  let union_map (f : 'a -> 'b t) (e : 'a t) : 'b t =
    let m = map f e in
    fold union m empty
end

let neg_to_zero n =
  if n < 0 then 0 else n

let div_ceil (n : int) (m : int) =
  if n mod m > 0 then n / m + 1 else n / m

let readfile (path : string) : string =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s