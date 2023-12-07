%{
  open Location
  open Syntax
%}

%token <Syntax.symbol> IDENT

%token LPAREN RPAREN
%token TRUE
%token FALSE
%token LARROW
%token LRARROW
%token LAND LOR LNEG
%token FOF
%token COMMA
%token DOT
%token EOF

%right    LARROW LRARROW
%left     LOR
%left     LAND
%left     LNEG

%type <Syntax.pform > xform
%type <Syntax.pproblem > xproblem

%start xform
%start xproblem
%%


(* -------------------------------------------------------------------- *)
xform:
| error
   { raise (ParseError (Some (Location.make $startpos $endpos), None)) }

| f=form EOF { f }

xproblem:
| error
   { raise (ParseError (Some (Location.make $startpos $endpos), None)) }

| clauses=list(clause) EOF { clauses }

(* -------------------------------------------------------------------- *)
%inline ident: x=loc(IDENT) { x }

(* -------------------------------------------------------------------- *)
form_r:
| f=parens(form_r)
    { f }

| TRUE
   { PFCst true }

| FALSE
   { PFCst false }

| x=ident
    { PFApp x }

| f1=form LAND f2=form
    { PFAnd (f1, f2) }

| f1=form LOR f2=form
    { PFOr (f1, f2) }

| f1=form LARROW f2=form
    { PFImp (f1, f2) }

| f1=form LRARROW f2=form
    { PFEquiv (f1, f2) }

| LNEG f=form
    { PFNot f }

form:
| f=loc(form_r) { f }

clause:
| FOF LPAREN name=ident COMMA role=ident COMMA f=form RPAREN DOT
    { { name; role; form = f } }

(* -------------------------------------------------------------------- *)
%inline loc(X):
| x=X {
    { pldesc = x;
      plloc  = Location.make $startpos $endpos;
    }
  }

(* -------------------------------------------------------------------- *)
%inline empty:
| (* empty *) { () }

(* -------------------------------------------------------------------- *)
%inline parens(X):
| LPAREN x=X RPAREN { x }

(* -------------------------------------------------------------------- *)
%inline plist0(X, S):
| aout=separated_list(S, X) { aout }

iplist1_r(X, S):
| x=X { [x] }
| xs=iplist1_r(X, S) S x=X { x :: xs }

%inline iplist1(X, S):
| xs=iplist1_r(X, S) { List.rev xs }

%inline plist1(X, S):
| aout=separated_nonempty_list(S, X) { aout }

%inline plist2(X, S):
| x=X S xs=plist1(X, S) { x :: xs }

%inline list2(X):
| x=X xs=X+ { x :: xs }
