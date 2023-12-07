open Engine.Fo
open Engine.Utils
open Location
open Syntax

type clause = {
  name : string;
  role : string;
  form : form;
}

type problem = clause list

let rec check (form : pform) : form =
  let pred name fs = FConn (name, List.map check fs) in
  match unloc form with
  | PFCst   true     -> FTrue
  | PFCst   false    -> FFalse
  | PFAnd   (f1, f2) -> pred `And   [f1; f2]
  | PFOr    (f1, f2) -> pred `Or    [f1; f2]
  | PFImp   (f1, f2) -> pred `Imp   [f1; f2]
  | PFEquiv (f1, f2) -> pred `Equiv [f1; f2]
  | PFNot   f1       -> pred `Not   [f1]
  | PFApp name ->
      FPred (unloc name, [])

let problem (pclauses : pproblem) : form =
  let (hyps, conc) =
    pclauses |> List.fold_left begin fun (hyps, conc) pclause ->
      let clause = {
        name = unloc pclause.Syntax.name;
        role = unloc pclause.Syntax.role;
        form = check pclause.form;
      } in
      match clause.role with
      | "axiom" -> (clause.form :: hyps, conc)
      | "hypothesis" -> (clause.form :: hyps, conc)
      | "lemma" -> (clause.form :: hyps, conc)
      | "conjecture" -> (hyps, clause.form :: conc)
      | _ -> raise TypingError
    end ([], []) in
  let antecedant = Form.f_ands hyps in
  let consequent = Form.f_ands conc in
  FConn (`Imp, [antecedant; consequent])