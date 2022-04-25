(* -------------------------------------------------------------------- *)
open Location

exception ParseError of Location.t option * string option

(* -------------------------------------------------------------------- *)
type symbol  = string
type psymbol = symbol loced

(* -------------------------------------------------------------------- *)
type ptype_r =
  | PTUnit
  | PTVar  of psymbol
  | PTSum  of ptype * ptype
  | PTProd of ptype * ptype
  | PTRec  of psymbol * ptype

and ptype = ptype_r loced

(* -------------------------------------------------------------------- *)
type ptyident = psymbol * ptype

(* -------------------------------------------------------------------- *)
type pexpr_r =
  | PEVar of psymbol * int
  | PEApp of psymbol * pexpr list

and pexpr  = pexpr_r loced
and pnexpr = psymbol * pexpr

(* -------------------------------------------------------------------- *)
type pform_r =
  | PFApp    of psymbol * pexpr list
  | PFCst    of bool
  | PFAnd    of pform * pform
  | PFOr     of pform * pform
  | PFImp    of pform * pform
  | PFEquiv  of pform * pform
  | PFNot    of pform
  | PFForall of ptyident * pform
  | PFExists of ptyident * pform

and pform = pform_r loced

(* -------------------------------------------------------------------- *)
type parity     = ptype list
type psignature = parity * ptype

type pvar =
  | PProp of psymbol * parity
  | PFun  of psymbol * psignature
  | PVar  of psymbol * ptype
  | PExpr of psymbol * pexpr
  | PTVar of psymbol
  | PType of psymbol * ptype

type pgoal = pvar list * pform list * pform

