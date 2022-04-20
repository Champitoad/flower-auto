let identity = fun x -> x

let (|>>) f g = fun x -> g (f x)
let (<<|) f g = fun x -> f (g x)

let rec fixpoint (f : 'a -> 'a) x =
  let y = f x in
  if y = x then x else (fixpoint f) y

module List = struct
  include BatList

  let diff l l' =
    remove_if (fun a -> List.mem a l') l

  let to_string ?(sep = "; ") ?(left = "[") ?(right = "]") print =
    map print |>> String.concat sep |>> fun s -> left ^ s ^ right
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