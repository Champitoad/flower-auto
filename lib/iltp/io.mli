(* -------------------------------------------------------------------- *)
open Engine.Utils

(* -------------------------------------------------------------------- *)
type reader

(* -------------------------------------------------------------------- *)
val from_channel : name:string -> BIO.input -> reader
val from_file    : string -> reader
val from_string  : string -> reader
val finalize     : reader -> unit

(* -------------------------------------------------------------------- *)
val parse_form    : reader -> Syntax.pform
val parse_problem : reader -> Syntax.pproblem