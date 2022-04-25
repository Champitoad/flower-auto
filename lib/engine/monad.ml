
(** Useful monads *)

open Utils

module type Type = sig type t end

(** Basic monad *)

module type Core = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

(** Basic monad with an additional monoid structure *)

module type Plus = sig
  include Core
  val zero : 'a t
  val ( + ) : 'a t -> 'a t -> 'a t
end

(** Basic monad with an additional environment type *)

module type Env = sig
  include Core
  type env
end

(** Reader monad to carry some environment in read-only mode *)

module Reader (T : Type) : sig
  include Env with type 'a t = T.t -> 'a

  (* Monadic version of List.map *)
  val map : ('a -> 'b t) -> 'a list -> 'b list t
end = struct
  type env = T.t
  type 'a t = env -> 'a
  let return x = fun _st -> x
  let bind m f = fun st -> f (m st) st
  let ( >>= ) = bind
  let ( let* ) = bind
  let map f l = fun st ->
    List.map (fun x -> f x st) l
end

(** State monad to carry some environment in read-write mode *)

module State (T : Type) : sig
  include Env with type 'a t = T.t -> 'a * T.t

  (* Monadic version of List.iter and List.fold_left *)
  val fold : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
  val iter : ('a -> unit t) -> 'a list -> unit t

  val get : T.t t
  val put : T.t -> unit t
  val run : 'a t -> T.t -> 'a
end = struct
  type env = T.t
  type 'a t = env -> 'a * env
  let return x = fun st -> x, st
  let bind m f = fun st ->
    let x, st' = m st in
    f x st'
  let ( >>= ) = bind
  let ( let* ) = bind
  let fold f x l = fun st ->
    List.fold_left
      (fun (x, st) y -> f x y st)
      (x, st) l
  let iter f l = fold (fun _ y -> f y) () l
  let get = fun st -> (st, st)
  let put st = fun _ -> ((), st)
  let run m = m |>> fst
end

(** List monad to implement list comprehension *)

module List : Plus
  with type 'a t = 'a list =
struct
  type 'a t = 'a list
  let return x = [x]
  let bind m f =  List.concat (List.map f m)
  let ( >>= ) = bind
  let ( let* ) = bind
  let zero = []
  let ( + ) = ( @ )
end

module Option : sig
  include Core with type 'a t = 'a option

  val fold       : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val concat     : 'a t t -> 'a t
  val concat_map : ('a -> 'b t) -> 'a t -> 'b t
end = struct
  type 'a t = 'a option

  let fold f acc = function
    | None   -> acc
    | Some v -> f acc v
  
  let concat = function
    | Some (Some x) -> Some x
    | _ -> None
  
  let concat_map f x =
    Option.(concat (map f x))
  
  let return x =
    Some x
  
  let bind x f =
    concat_map f x

  let ( >>= ) =
    bind
  
  let ( let* ) =
    bind
end