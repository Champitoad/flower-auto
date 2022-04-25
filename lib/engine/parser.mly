%{
  open Location
  open Syntax
%}

%token <Syntax.symbol> IDENT
%token <int> NAT

%token LPAREN RPAREN
%token LBRACE RBRACE
%token TRUE
%token EXISTS
%token FALSE
%token FORALL
%token REC
%token TYPE
%token LARROW
%token LRARROW
%token EOF
%token PROOF

%token LAND LOR LNEG PLUS STAR
%token AMP DCOLON COLON EQ COLONEQ COMMA DOT SEMICOLON

%nonassoc BINDING_prec
%right    LARROW LRARROW
%nonassoc REC_prec
%left     LOR  PLUS
%left     LAND STAR
%left     LNEG

%type <Syntax.ptype > xtype
%type <Syntax.pexpr > xexpr
%type <Syntax.pnexpr> xnexpr
%type <Syntax.pform > xform
%type <Syntax.pgoal > xgoal

%start xtype
%start xexpr
%start xnexpr
%start xform
%start xgoal
%%

(* -------------------------------------------------------------------- *)
xtype:
| error
    { raise (ParseError (Some (Location.make $startpos $endpos), None)) }

| t=type_ EOF { t }

(* -------------------------------------------------------------------- *)
xexpr:
| error
    { raise (ParseError (Some (Location.make $startpos $endpos), None)) }

| e=expr EOF { e }

(* -------------------------------------------------------------------- *)
xnexpr:
| error
    { raise (ParseError (Some (Location.make $startpos $endpos), None)) }

| e=nexpr EOF { e }

(* -------------------------------------------------------------------- *)
xform:
| error
   { raise (ParseError (Some (Location.make $startpos $endpos), None)) }

| f=form EOF { f }

(* -------------------------------------------------------------------- *)
xgoal:
| error
   { raise (ParseError (Some (Location.make $startpos $endpos), None)) }

| p=goal EOF { p }

(* -------------------------------------------------------------------- *)
%inline ident: x=loc(IDENT) { x }

tyident:
| x=ident COLON ty=type_ { (x, ty) }

(* -------------------------------------------------------------------- *)
type_r:
| t=parens(type_r)
    { t }

| parens(empty)
    { PTUnit }

| x=ident
    { PTVar x }

| t1=type_ PLUS t2=type_
    { PTSum (t1, t2) }

| t1=type_ STAR t2=type_
    { PTProd (t1, t2) }

| REC x=ident DOT t=type_ %prec REC_prec
    { PTRec (x, t) }

type_:
| t=loc(type_r) { t }

(* -------------------------------------------------------------------- *)
unparens_expr_r:
| x=ident
    { PEVar (x, 0) }

| x=ident LBRACE i=NAT RBRACE
    { PEVar (x, i) }

| f=ident parens(empty)
    { PEApp (f, []) }

| f=ident args=parens(plist1(expr, COMMA))
    { PEApp (f, args) }

expr_r:
| e=unparens_expr_r
    { e }
| e=parens(expr_r)
    { e }

unparens_expr:
| e=loc(unparens_expr_r) { e }

expr:
| e=loc(expr_r) { e }

(* -------------------------------------------------------------------- *)
nexpr:
| x=ident COLONEQ e=expr { (x, e) }

(* -------------------------------------------------------------------- *)
form_r:
| f=parens(form_r)
    { f }

| TRUE
   { PFCst true }

| FALSE
   { PFCst false }

| x=ident
    { PFApp (x, []) }

| e1=unparens_expr EQ e2=unparens_expr
    { PFApp (mkloc _dummy "_EQ", [e1; e2]) }

| x=ident args=parens(plist1(expr, COMMA))
    { PFApp (x, args) }

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

| FORALL xty=tyident DOT f=form %prec BINDING_prec
    { PFForall (xty, f) }

| EXISTS xty=tyident DOT f=form %prec BINDING_prec
    { PFExists (xty, f) }

form:
| f=loc(form_r) { f }

(* -------------------------------------------------------------------- *)
%inline arity:
| tys=plist0(type_, AMP)
    { tys }

signature:
| ar=arity LARROW ty=type_
    { (ar, ty) }

(* -------------------------------------------------------------------- *)
entry:
| x=ident
    { PProp (x, []) }

| x=ident DCOLON xty=arity
    { PProp (x, xty) }

| x=ident COLON xty=signature
    { PFun (x, xty) }

| x=ident COLON xty=type_
    { PVar (x, xty) }

| x=ident COLONEQ body=expr
    { PExpr (x, body) }

| TYPE a=ident
    { PTVar a }

| TYPE a=ident COLONEQ t=type_
    { PType (a, t) }

(* -------------------------------------------------------------------- *)
goal:
| ps=plist0(entry, COMMA) hs=option(preceded(SEMICOLON, plist0(form, COMMA))) PROOF f=form
    { (ps, BatOption.default [] hs, f) }

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
