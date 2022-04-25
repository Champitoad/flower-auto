(* -------------------------------------------------------------------- *)
open Utils

(* -------------------------------------------------------------------- *)
type reader

(* -------------------------------------------------------------------- *)
val from_channel : name:string -> BIO.input -> reader
val from_file    : string -> reader
val from_string  : string -> reader
val finalize     : reader -> unit

(* -------------------------------------------------------------------- *)
val parse_type  : reader -> Syntax.ptype
val parse_expr  : reader -> Syntax.pexpr
val parse_nexpr : reader -> Syntax.pnexpr
val parse_form  : reader -> Syntax.pform
val parse_goal  : reader -> Syntax.pgoal
