open Utils

(* -------------------------------------------------------------------- *)
(** Syntax *)

type name = string

type flower =
  | Atom of name
  | Flower of garden * garden list
and garden = flower list

let atom_of s =
  Atom s

let atoms_of =
  List.map atom_of

let atomic = function
  | Atom _ -> true
  | _ -> false

let atom = function
  | Atom a -> a
  | _ -> failwith "Non-atomic flower"

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
      Printf.sprintf "(%s ⫐ %s)"
        (string_of_garden p)
        (ps |> List.map (string_of_garden |>> fun s -> s ^ ";") |> String.concat " ")

and string_of_garden g =
  List.to_string ~sep:", " ~left:"" ~right:"" string_of_flower g

let rec latex_of_flower = function
  | Atom a -> a
  | Flower (p, ps) ->
      Printf.sprintf "(\\flower{%s}{%s})"
        (latex_of_garden p)
        (ps |> List.map (latex_of_garden |>> fun s -> s ^ "\\sep") |> String.concat " ")

and latex_of_garden g =
  (* if List.is_empty g then "{}" else *)
  List.to_string ~sep:", " ~left:"" ~right:"" latex_of_flower g

let string_of_garden_path, string_of_flower_path =
  let rec aux_f (f : flower) (sub : Itree.path) : string =
    match sub with
    | [] ->
        Printf.sprintf "\027[1;31m%s\027[0m" (string_of_flower f)
    | i :: sub ->
        match f with
        | Atom a -> a
        | Flower (p, ps) ->
            let gs = 
              p :: ps |> List.mapi begin fun j g ->
                let s =
                  if i = j then aux_g g sub
                  else string_of_garden g in
                if j = 0 then s else s ^ ";"
              end in
            let pistil = List.hd gs in
            let petals = List.tl gs |> String.concat " " in
            Printf.sprintf "(%s ⫐ %s)" pistil petals

  and aux_g (g : garden) (sub : Itree.path) : string =
    match sub with
    | [] ->
        Printf.sprintf "\027[1;32m%s\027[0m" (string_of_garden g)
    | i :: sub ->
        let g = 
          g |> List.mapi begin fun j f ->
            if i = j then aux_f f sub
            else string_of_flower f
          end in
        List.to_string ~sep:", " ~left:"" ~right:"" identity g
  
  in aux_g, aux_f

(* -------------------------------------------------------------------- *)
(** Conversion to/from formulas *)

let rec of_form (f : Engine.Fo.form) : garden =
  let open Engine.Fo in
  match f with
  | FPred (a, []) ->
      [Atom a]
  | FTrue ->
      []
  | FFalse ->
      [Flower ([], [])]
  | FConn (`And, [f1; f2]) ->
      of_form f1 @ of_form f2
  | FConn (`Or, [f1; f2]) ->
      [Flower ([], [of_form f1; of_form f2])]
  | FConn (`Imp, [f1; f2]) ->
      [Flower (of_form f1, [of_form f2])]
  | FConn (`Not, [f1]) ->
      [Flower (of_form f1, [])]
  | FConn (`Equiv, [f1; f2]) ->
      [Flower (of_form f1, [of_form f2])] @
      [Flower (of_form f2, [of_form f1])]
  | _ ->
      failwith "Non-propositional formula"

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
  
  (** In order to cover flowers where all subflowers are different, we can
      lower-bound the pool of atom identifiers by an upper bound on the
      size of the generated flower. The closer it is to the least upper
      bound, the likier it is to generate a correct flower. *)
  let upper_bound (params : params) : int =
    (* BatInt.pow ((params.max_petals + 1) * params.max_flowers) params.depth / 500 *)
    5

  let gen_flower ?(params = defaults) () =
    gen_flower (upper_bound params) params

  let gen_garden ?(params = defaults) () =
    gen_garden (upper_bound params) params
  
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
        with _ -> [] in

      let flowers = List.init nb_flowers
        (fun _ -> gen_flower ctx params) in
      
      List.map atom_of atoms @ flowers
    
    let gen_flower ?(params = defaults) () =
      gen_flower [] params

    let gen_garden ?(params = defaults) () =
      gen_garden [] params
  end
end

(* -------------------------------------------------------------------- *)
(** Proof search *)

let empty_pistil : flower -> bool = function
  | Flower ([], _) -> true
  | _ -> false

exception Sterile

(** [reproduction_data f] searches for the first stamen [scrutinee] with an
    empty pistil inside the pistil of [f], and returns the pattern-matching
    data associated with it in the form of a triplet [(hyps, lhs, rhs)], where:
    - [hyps] are the other stamens in the pistil of [f]
    - [lhs] are the petals of [scrutinee]
    - [rhs] are the petals of [f]
    
    @raise Sterile [f] if there is no such stamen. *)
let reproduction_data (f : flower) : garden * garden list * garden list =
  match f with
  | Atom _ -> raise Sterile
  | Flower (p, _) ->
      let scrutinee =
        begin try
          List.find empty_pistil p
          with Not_found -> raise Sterile
        end in
      List.remove p scrutinee, petals scrutinee, petals f

let reproducible (f : flower) : bool =
  try
    let _ = reproduction_data f in
    true
  with Sterile -> false

let assumptions_flower, assumptions_garden =
  let rec aux_f (ctx : flower Set.t) (f : flower) : name Set.t =
    (* Printf.printf "%s : %s\n"
      (ctx |> Set.to_list |> string_of_garden)
      (f |> string_of_flower); *)
    match f with
    (* Insemination *)
    | _ when Set.mem f ctx -> Set.empty
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
        if List.is_empty ps then
          Set.singleton "⊥"
        else
          let assums = aux_g ctx p |> Set.map atom_of in
          let ctx = Set.([ctx; Set.of_list p; assums] |> of_list |> unionf) in
          let ps = List.map (aux_g ctx) ps in
          if List.exists Set.is_empty ps then
            Set.empty
          else
            ps |> Set.of_list |> Set.unionf

  and aux_g (ctx : flower Set.t) : garden -> name Set.t = function
    (* Garden *)
    | fs ->
        fs |> Set.of_list |> Set.union_map begin fun f ->
          let pruned = List.remove fs f |> Set.of_list in
          aux_f (Set.union ctx pruned) f
        end

  in (aux_f Set.empty, aux_g Set.empty)

let check_flower, check_garden =
  (assumptions_flower |>> Set.is_empty,
   assumptions_garden |>> Set.is_empty)

let reproduction (g : garden) : garden =
  let rec aux_f f =
    if reproducible f then
      let hyps, lhs, rhs = reproduction_data f in
      let reproduction =
        let branches = lhs |> List.map (fun p -> Flower (p, rhs)) in
        Flower (hyps, [branches]) in
      aux_f reproduction
    else
      match f with
      | Flower (p, ps) -> Flower (aux_g p, List.map aux_g ps)
      | _ -> f
  and aux_g g =
    List.map aux_f g
  in aux_g g

let decomposition (g : garden) : garden =
  let rec aux_f f =
    match f with
    | Atom _ -> [f]
    | Flower ([Flower ([Atom _ as a], [])], []) -> [a]
    | Flower (_, ps) when List.(exists is_empty ps) -> []
    | Flower ([], [p]) -> aux_g p
    | Flower (p, ps) -> [Flower (aux_g p, List.map aux_g ps)]
  and aux_g g =
    List.concat_map aux_f g
  in aux_g g

let pollination (g : garden) : garden =
  let rec aux_f (ctx : flower Set.t) f =
    match f with
    | Flower (p, ps) ->
        let ctx' = Set.(union ctx (of_list p)) in
        let pistil = aux_g ctx p in
        let petals = List.map (aux_g ctx') ps in
        Flower (pistil, petals)
    | Atom _ -> f
  and aux_g ctx g =
    (* Printf.printf "%s : %s\n"
      (ctx |> Set.to_list |> string_of_garden)
      (g |> string_of_garden); *)
    let g = Set.(diff (of_list g) ctx) in
    let ctx = Set.map (aux_f g) ctx in
    let ctx = Set.union ctx g in
    Set.(map (fun f -> aux_f (remove f ctx) f) g |> to_list)
  in aux_g Set.empty g

(* -------------------------------------------------------------------- *)
(** Efficient pollination algorithm *)

type pol = int

let positive (p : pol) =
  p mod 2 = 0

let negative (p : pol) =
  p mod 2 = 1

type kind = [`Flower | `Garden]
type zone = [`Pistil | `Petal]

type pmdata =
  { pol : pol ; kind : kind; zone : zone }

type amdata =
  { name : name; mutable justifiber : gtree list }

and gtree = (pmdata, amdata) Itree.t

type vehicle =
  { pos : gtree list; neg : gtree list }

let string_of_amdata { name; justifiber } : string =
  if not (List.is_empty justifiber) then
    Printf.sprintf "\027[1;32m%s\027[0m" name
  else
    Printf.sprintf "\027[1;31m%s\027[0m" name

let string_of_pol p =
  if positive p then "+" else "-"

let string_of_kind = function
  | `Flower -> "F" | `Garden -> "G"

let string_of_pmdata { pol; kind; zone } : string =
  let pol = string_of_pol pol in
  let kind = string_of_kind kind in
  let polkind = pol ^ kind in
  match zone with
  | `Pistil -> polkind ^ "⫐"
  | `Petal -> "⫐" ^ polkind

let string_of_gtree : gtree -> string =
  Itree.to_string string_of_amdata string_of_pmdata

let string_of_vehicle (v : vehicle) : string =
  let string_of_atom (t : gtree) =
    let amdata = Itree.leaf_data t in
    let { pol; _ } = Itree.node_data t in
    Printf.sprintf "%s%s"
      (string_of_pol pol)
      (string_of_amdata amdata) in

  let string_of_atoms =
    List.to_string ~left:"" ~right:"" ~sep:", " string_of_atom in

  Printf.sprintf "%s | %s"
    (string_of_atoms v.pos)
    (string_of_atoms v.neg)
    

exception NotAnAtom of gtree
exception NotAFlower of gtree
exception NotAGarden of gtree

let kind (t : gtree) : kind =
  match t.data with
  | Leaf ({ kind; _ }, _) ->
      if kind = `Garden then
        failwith "Leaves cannot be gardens";
      kind
  | Node ({ kind; _ }) -> kind

let atom (t : gtree) : gtree =
  match t.data with
  | Leaf ({ kind = `Flower; _ }, _) -> t
  | _ -> raise (NotAnAtom t)

let flower (t : gtree) : gtree =
  match t.data with
  | Node ({ kind = `Flower; _ }) -> t
  | _ -> raise (NotAFlower t)

let garden (t : gtree) : gtree =
  match t.data with
  | Node ({ kind = `Garden; _ }) -> t
  | _ -> raise (NotAGarden t)

let name (t : gtree) : name =
  (t |> atom |> Itree.leaf_data).name

let justifiber (t : gtree) : gtree list =
  (t |> atom |> Itree.leaf_data).justifiber

let pistil (t : gtree) : gtree =
  Itree.child (flower t) 0

let petals (t : gtree) : gtree list =
  BatDynArray.(tail (flower t).children 1 |> to_list)

let flowers (t : gtree) : gtree list =
  (garden t).children |> BatDynArray.to_list

let eq_vehicle (v : vehicle) (v' : vehicle) : bool =
  Utils.List.same_elements_q v.pos v'.pos &&
  Utils.List.same_elements_q v.neg v'.neg

let append_vehicle { pos = p; neg = n } { pos = p'; neg = n' } =
  { pos = p @ p'; neg = n @ n' }

let concat_vehicle =
  List.fold_left append_vehicle { pos = []; neg = [] }

let vehicle (t : gtree) : vehicle =
  let rec aux (t : gtree) : vehicle =
    match t.data with
    | Leaf ({ pol; _ }, _) ->
        let pos, neg = if positive pol then [t], [] else [], [t] in
        { pos; neg }
    | Node (_) ->
        t.children |> BatDynArray.to_list |>
        List.map aux |> concat_vehicle
  in aux t

let eq_atom (t : gtree) (t' : gtree) : bool =
  Itree.(leaf_data t = leaf_data t')

let justify_atom (src : gtree) (tgt : gtree) : unit =
  tgt.data <- Itree.(Leaf (node_data tgt,
    { (leaf_data tgt) with justifiber = src :: justifiber tgt }))

let eq_gtree (t : gtree) (t' : gtree) : bool =
  let eq_data d d' =
    let open Itree in
    match d, d' with
    | Leaf (nd, ad), Leaf (nd', ad') ->
        nd = nd' &&
        ad.name = ad'.name
    | Node nd, Node nd' ->
        nd = nd'
    | _ ->
        false in
  Itree.eq ~eq_data t t'

let deepcopy_gtree (t : gtree) : gtree =
  let open Itree in
  let copy_data d =
    match d with
    | Leaf (nd, ad) ->
        Leaf (nd, { ad with justifiber = ad.justifiber })
    | _ -> d in 
  deepcopy ~copy_data t

let garden_to_gtree (g : garden) : gtree =
  let rec build_f (index : int) (pmdata : pmdata) (f : flower) : gtree =
    match f with
    | Atom name ->
        let data = Itree.Leaf ({ pmdata with kind = `Flower }, { name; justifiber = []; }) in
        { parent = None; index; children = BatDynArray.create (); data }
    | Flower (p, ps) ->
        let children =
          let pistil =
            let t = build_g 0 { pmdata with pol = pmdata.pol + 1; zone = `Pistil } p in
            BatDynArray.of_list [t] in
          let petals =
            let ts =
              ps |> List.mapi begin fun i p ->
                build_g (i+1) { pmdata with zone = `Petal } p
              end in
            BatDynArray.of_list ts in
          BatDynArray.append petals pistil;
          pistil in
        let data = Itree.Node ({ pmdata with kind = `Flower }) in
        { parent = None; index; children; data }

  and build_g (index : int) (pmdata : pmdata) (g : garden) : gtree =
    let children =
      g |> List.mapi (fun i f -> build_f i pmdata f) |> BatDynArray.of_list in
    let data = Itree.Node ({ pmdata with kind = `Garden }) in
    { parent = None; index; children; data }
  in

  let t = build_g 0 { pol = 0; kind = `Garden; zone = `Petal } g in
  Itree.link t;
  t

let gtree_to_garden (t : gtree) : garden =
  let rec aux_f (t : gtree) : flower =
    try
      Atom (name t)
    with NotAnAtom _ ->
      let pistil = t |> pistil |> aux_g in
      let petals = t |> petals |> List.map aux_g in
      Flower (pistil, petals)
  and aux_g (t : gtree) : garden =
    (flowers t) |> List.map aux_f
  in
  match kind t with
  | `Flower -> [aux_f t]
  | `Garden -> aux_g t

let string_of_node (t : gtree) : string =
  string_of_garden_path (gtree_to_garden (Itree.root t)) (Itree.path t)

let latex_of_node (t : gtree) : string =
  latex_of_garden (gtree_to_garden t)

let empty_pistil (t : gtree) : bool =
  try BatDynArray.empty (pistil t).children
  with NotAFlower _ -> false

let empty_garden (t : gtree) : bool =
  List.is_empty (flowers t)

let reproduction (t : gtree) : unit =
  let scrutinee (t : gtree) : gtree =
    try BatDynArray.find empty_pistil (pistil t).children
    with Not_found -> raise Sterile
  in
  let rec aux (t : gtree) : unit =
    try
      let _ = atom t in ()
    with NotAnAtom u when u == t ->
      try 
        Itree.link t;
        
        let scrutinee = scrutinee t in
        let rhs = petals t in
        let pol = (Itree.node_data t).pol in

        let branches : gtree list =
          petals scrutinee |> List.mapi begin fun i p ->
            let rhs = List.map deepcopy_gtree rhs in
            let children = BatDynArray.of_list (Itree.{ p with index = 0 } :: rhs) in
            let data = Itree.(Node ({ pol; kind = `Flower; zone = `Petal })) in
            Itree.{ parent = None; index = i; children; data }
          end in

        let petal =
          let children = branches |> BatDynArray.of_list in
          let data = Itree.(Node ({ pol; kind = `Garden; zone = `Petal })) in
          Itree.{ parent = None; index = 1; children; data } in
        
        (* Remove scrutinee *)
        Itree.remove_child (pistil t) scrutinee.index;
        (* Remove petals *)
        for i = (BatDynArray.length t.children - 1) downto 1 do
          Itree.remove_child t i
        done;
        (* Replace them with the new petal containing the branches *)
        Itree.insert_child t 1 petal;
        Itree.link t;
        (* Repeat *)
        aux t
      with
      | Sterile ->
          (* aux (pistil t); *)
          List.iter aux (petals t)
      | NotAFlower u when u == t ->
          List.iter aux (flowers t)
  in aux t

let decomposition (t : gtree) : unit =
  let rec aux (t : gtree) : unit =
    try
      (* Atom *)
      let _ = atom t in ()
    with NotAnAtom _ ->
      try
        let petals = petals t in
        let parent = t.parent |> Option.get in
        (* Empty petal *)
        if List.(exists empty_garden petals) then begin
          Itree.remove_child parent t.index;
          Itree.link parent
        end
        else begin
          (* match empty_pistil t, petals with *)
          (* Empty pistil *)
          (* | true, [g] ->
              Itree.remove_child parent t.index;
              flowers g |> List.iter begin fun f ->
                Itree.insert_child parent t.index f;
                Itree.link parent;
                aux f;
              end *)
          (* Default flower *)
          (* | _ -> *)
              aux (pistil t);
              Itree.link parent;
              List.iter (fun c -> aux c; Itree.link parent) petals;
              if List.(exists empty_garden petals) then begin
                Itree.remove_child parent t.index;
                Itree.link parent
              end
        end;
      (* Garden *)
      with NotAFlower _ ->
        List.iter (fun c -> aux c; Itree.link t) (flowers t)
  in aux t

let pollination (t : gtree) : unit =
  let v = vehicle t in
  v.pos |> List.iter begin fun p ->
    v.neg |> List.iter begin fun n ->
      if name p = name n && Itree.in_same_tree p n then begin
        let anc = Itree.lca p n in
        let { pol; kind; _ } = Itree.node_data anc in
        let src, tgt = n, p in

        let valid =
          let tgt_unjustified = not (List.memq src (justifiber tgt)) in
          tgt_unjustified && begin
            let garden_and_negative = kind = `Garden && negative pol in
            garden_and_negative || begin
              let flower_and_positive = kind = `Flower && positive pol in
              flower_and_positive && begin
                let pistil = pistil anc in
                let src_top_pistil = BatDynArray.memq src pistil.children in
                let src_in_petal = Itree.is_desc pistil tgt in
                src_top_pistil || src_in_petal
              end
            end
          end in

        if valid then begin
          Printf.printf "[src]: %s\n" (string_of_node src);
          Printf.printf "[tgt]: %s\n" (string_of_node tgt);
          (* Compute path from ancestor to target *)
          let path_anc_tgt = Itree.path ~stop:(anc.parent) tgt in

          let src_available =
            let parent =
              match kind with
              | `Garden -> Option.get src.parent
              | `Flower -> Option.get (Option.get src.parent).parent in
            parent == anc in

          let new_anc = ref anc in
          let path_new_anc_tgt = ref path_anc_tgt in

          (* If the source is not directly available at the top-level of the ancestor *)
          if not src_available then begin
            (* Make a copy of the direct subflower of the ancestor containing the target *)
            let subflower =
              let dist_from_anc =
                match kind with
                | `Flower -> 2
                | `Garden -> 1 in
              let path_from_anc = List.split_at dist_from_anc path_anc_tgt |> fst in
              deepcopy_gtree (Itree.desc anc path_from_anc) in

            (* Attach the copy to its new location besides the source *)
            new_anc := src.parent |> Option.get;
            Itree.insert_child !new_anc 0 subflower;
            let tl = match kind with
              | `Garden -> List.tl path_anc_tgt
              | `Flower -> List.tl (List.tl path_anc_tgt) in
            path_new_anc_tgt := 0 :: tl;
          end;

          (* Remove the (copy of) the target *)
          let new_tgt = Itree.desc !new_anc !path_new_anc_tgt in
          let parent = new_tgt.parent |> Option.get in
          Itree.remove_child parent new_tgt.index;

          justify_atom src tgt;
        end;
      end;
    end
  end

(** Correctness criterion *)

let lifecycle ?(printer = None) (t : gtree) : unit =
  let print phase =
    match printer with
    | Some p -> Printf.printf "%s%s\n" phase (p t) 
    | None -> ()
  in
  pollination t;
  print "[pollination]:   ";
  reproduction t;
  print "[reproduction]:  ";
  decomposition t;
  print "[decomposition]: "

let life ?(printer = None) (g : garden) : garden =
  let t = ref (garden_to_gtree g) in
  let t' = ref (deepcopy_gtree !t) in
  lifecycle ~printer !t;
  while not (eq_gtree !t !t') do
    t' := deepcopy_gtree !t;
    lifecycle ~printer !t;
  done;
  gtree_to_garden !t

let check ?(printer = None) : garden -> bool =
  life ~printer |>> List.is_empty