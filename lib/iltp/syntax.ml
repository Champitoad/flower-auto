(* -------------------------------------------------------------------- *)
open Location

exception ParseError of Location.t option * string option

(* -------------------------------------------------------------------- *)
type symbol  = string
type psymbol = symbol loced

(* -------------------------------------------------------------------- *)
type pform_r =
  | PFCst    of bool
  | PFApp    of psymbol
  | PFAnd    of pform * pform
  | PFOr     of pform * pform
  | PFImp    of pform * pform
  | PFEquiv  of pform * pform
  | PFNot    of pform

and pform = pform_r loced

type pclause = {
  name : symbol loced;
  role : string loced;
  form : pform;
}

type pproblem = pclause list
