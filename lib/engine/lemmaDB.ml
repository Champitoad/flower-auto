open Utils

type t = {
  db_env : Fo.env;
  db_map : (string, Fo.form) Map.t;
}

let empty env =
  { db_env = env; db_map = Map.empty }

exception LemmaNotFound of string

let find db name =
  Option.get_exn
    (Map.find_opt name db.db_map)
    (LemmaNotFound name)

let add db ~(name:string) ~(form:string) =
  let form = Io.parse_form (Io.from_string form) in
  let form = Fo.Form.check db.db_env form in
  { db with db_map = Map.add name form db.db_map }
  
let load db lemmas =
  List.fold (fun db (name, form) -> add db ~name ~form) db lemmas

let all db =
  Map.bindings db.db_map