open Utils

(* -------------------------------------------------------------------- *)
(** Syntax *)

type flower =
  | Atom of string
  | Flower of garden * garden list
and garden = flower list

let atom_of s =
  Atom s

let atoms_of =
  List.map atom_of

let pistil = function
  | Atom _ -> failwith "Atomic flowers do not have a pistil"
  | Flower (p, _) -> p

let petals = function
  | Atom _ -> failwith "Atomic flowers do not have petals"
  | Flower (_, ps) -> ps

(* -------------------------------------------------------------------- *)
(** Print flowers in textual notation *)

let rec string_of_flower = function
  | Atom a -> a
  | Flower (p, ps) ->
      Printf.sprintf "(%s ⊢ %s)"
        (string_of_garden p)
        (ps |> List.map (string_of_garden |>> fun s -> s ^ ";") |> String.concat " ")

and string_of_garden g =
  List.to_string ~sep:", " ~left:"" ~right:"" string_of_flower g

(* -------------------------------------------------------------------- *)
(** Generate random flowers *)
module Random : sig
  type params = {
    depth : int;
    max_flowers : int;
    max_petals : int;
  }

  val defaults : params

  val reset_freshness : unit -> unit

  (** [gen_flower { depth; max_flowers; max_petals } ()] generates a random
      flower of depth at most [depth], where each subgarden contains between 0
      and [max_flowers] flowers, and each subflower between 0 and [max_petals]
      petals *)
  val gen_flower : ?params:params -> unit -> flower

  (** [gen_garden { depth; max_flowers; max_petals } ()] generates a random
      garden of depth at most [depth], where each subgarden contains between 0
      and [max_flowers] flowers, and each subflower between 0 and [max_petals]
      petals *)
  val gen_garden : ?params:params -> unit -> garden

  (** Generate only (eta-expanded) correct flowers *)
  module Correct : sig

    val gen_flower : ?params:params -> unit -> flower

    val gen_garden : ?params:params -> unit -> garden

  end
end = struct
  type params = {
    depth : int;
    max_flowers : int;
    max_petals : int;
  }
  let defaults = {
    depth = 3;
    max_flowers = 4;
    max_petals = 4;
  }

  let fresh_name, reset_freshness =
    let id = ref (-1) in
    let fresh_name () =
      id := !id+1;
      Printf.sprintf "%d" !id in
    let reset_freshness () =
      id := -1 in
    fresh_name, reset_freshness
  
  (** [pick_atom bound] picks a random atom identifier among a pool of
      identifers of size [bound] *)
  let pick_atom bound =
    Random.int bound |> string_of_int

  let rec gen_flower bound params =
    match params.depth with
    | 0 -> Atom (pick_atom bound)
    | _ ->
        let params = { params with depth = params.depth - 1 } in
        let pistil = gen_garden bound params in
        let petals = List.init
          (Random.int params.max_petals)
          (fun _ -> gen_garden bound params) in
        Flower (pistil, petals)

  and gen_garden bound params =
    List.init
      (Random.int params.max_flowers)
      (fun _ -> gen_flower bound params)
  
  let gen_flower ?(params = defaults) () =
    (* In order to cover flowers where all subflowers are different, we can
        lower-bound the pool of atom identifiers by an upper bound on the
        size of the generated flower. The closer it is to the least upper
        bound, the likier it is to generate a correct flower. *)
    let upper_bound =
      BatInt.pow ((params.max_petals + 1) * params.max_flowers) params.depth in
    gen_flower upper_bound params

  let gen_garden ?(params = defaults) () =
    let upper_bound =
      BatInt.pow ((params.max_petals + 1) * params.max_flowers) params.depth in
    gen_garden upper_bound params
  
  module Correct = struct
    let rec gen_flower ctx params =
      match params.depth with
      | 0 -> failwith "An atomic flower cannot be correct"
      | _ ->
          let params = { params with depth = params.depth - 1 } in

          (* Generate fresh atomic hypotheses in pistils, and add them to the
             pollination context [ctx].

             We could also add arbitrary, useless non-atomic hypotheses to cover
             more correct flowers, but then they must not be added to the
             context in order to stay eta-expanded *)
          let nb_hyps = Random.int (params.max_flowers/2 + 2) in
          let new_hyps = List.init nb_hyps (fun _ -> fresh_name ()) in
          let ctx = ctx @ new_hyps in
          let pistil =
            List.map atom_of new_hyps @
            let max_flowers = neg_to_zero (params.max_flowers - nb_hyps) + 1 in
            gen_garden ctx { params with max_flowers } in

          (* Generate one correct petal, and otherwise arbitrary, useless petals *)
          let petals =
            let correct_one = gen_garden ctx params in
            let others =
              let nb_petals = Random.int (neg_to_zero (params.max_petals - 1) + 1) in
              List.init nb_petals
                (fun _ -> gen_garden ctx params) in
            correct_one :: others in

          Flower (pistil, petals)

    and gen_garden ctx params =
      let nb_atoms, nb_flowers =
        match params.depth with
        | 0 -> Random.int (params.max_flowers + 1), 0
        | _ -> Random.int (params.max_flowers/2 + 1), Random.int (div_ceil params.max_flowers 2 + 1)
        in

      let atoms =
        try List.(init nb_atoms
            (fun _ -> nth ctx (Random.int (length ctx + 1))))
        (* The empty garden is the only correct garden in an empty context *)
        with Failure _ -> [] in

      let flowers = List.init nb_flowers
        (fun _ -> gen_flower ctx params) in
      
      List.map atom_of atoms @ flowers
    
    let gen_flower ?(params = defaults) () =
      gen_flower [] params

    let gen_garden ?(params = defaults) () =
      gen_garden [] params
  end
end

(* Correctness criterion *)

let empty_pistil : flower -> bool = function
  | Flower ([], _) -> true
  | _ -> false

exception Sterile of flower

(** [reproduction_data f] searches for the first stamen [scrutinee] with an
    empty pistil inside the pistil of [f], and returns the pattern-matching
    data associated with it in the form of a triplet [(hyps, lhs, rhs)], where:
    - [hyps] are the other stamens in the pistil of [f]
    - [lhs] are the petals of [scrutinee]
    - [rhs] are the petals of [f]
    
    @raise Sterile [f] if there is no such stamen. *)
let reproduction_data (f : flower) : garden * garden list * garden list =
  match f with
  | Atom _ -> raise (Sterile f)
  | Flower (p, _) ->
      let scrutinee =
        begin try
          List.find empty_pistil p
          with Not_found -> raise (Sterile f)
        end in
      BatList.remove p scrutinee, petals scrutinee, petals f

let reproducible (f : flower) : bool =
  try
    let _ = reproduction_data f in
    true
  with Sterile _ -> false

let assumptions_flower, assumptions_garden =
  let rec aux_f (ctx : flower Set.t) (f : flower) : string Set.t =
    (* Printf.printf "%s : %s\n"
      (ctx |> Set.to_list |> string_of_garden)
      (f |> string_of_flower); *)
    match f with
    (* Insemination *)
    | _ when BatSet.mem f ctx -> Set.empty
    (* Stem *)
    | Atom a -> Set.singleton a
    (* Reproduction *)
    | f when reproducible f ->
        let hyps, lhs, rhs = reproduction_data f in
        let reproduction =
          let branches = List.map (fun p -> Flower (p, rhs)) lhs in
          Flower (hyps, [branches]) in
        aux_f ctx reproduction
    (* Transport *)
    | Flower (p, ps) ->
        let assums = aux_g ctx p |> Set.map atom_of in
        ps |> List.map (fun p -> aux_g (Set.union ctx assums) p) |>
        Set.of_list |> Set.intersectionf "⊥"

  and aux_g (ctx : flower Set.t) : garden -> string Set.t = function
    (* Garden *)
    | fs ->
        fs |> Set.of_list |> Set.union_map begin fun f ->
          let pruned = BatList.remove fs f |> Set.of_list in
          aux_f (Set.union ctx pruned) f
        end

  in (aux_f Set.empty, aux_g Set.empty)

let check_flower, check_garden =
  (assumptions_flower |>> Set.is_empty,
   assumptions_garden |>> Set.is_empty)