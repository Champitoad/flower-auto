open Engine
open Fo

let parse_tautos ic : (string * form list) list =
  BatIO.lines_of ic |>
  BatEnum.filter begin fun l ->
    not (BatString.is_empty l) &&
    let is_section = BatString.sub l 0 3 = "###" in
    let starts_with_hash = BatString.sub l 0 1 = "#" in
     not starts_with_hash || is_section
  end |>
  BatList.of_enum |>
  List.fold_left begin fun acc l ->
    let l = BatString.trim l in
    let is_section = BatString.sub l 0 3 = "###" in
    if is_section then
      let title = BatString.sub l 4 (BatString.length l - 4) in
      (title, []) :: acc
    else
      let goal = Io.parse_goal (Io.from_string l) in
      let _, _, form = Fo.Goal.check goal in
      let (title, fs) = List.hd acc in
      (title, form :: fs) :: List.tl acc
  end []
  
exception UnsupportedConnective of logcon * int
exception UnsupportedFOL

let rec latex_of_form (f : form) : string =
  match f with
  | FTrue ->
      {|\top|}
  | FFalse ->
      {|\bot|}
  | FPred (p, []) ->
      p
  | FPred _ ->
      raise UnsupportedFOL

  | FConn (c, [f1; f2]) ->
    let f1 = latex_of_form f1 in
    let f2 = latex_of_form f2 in
    let conn =
      begin match c with
      | `And -> {|\land|}
      | `Or -> {|\lor|}
      | `Imp -> {|\limp|}
      | `Equiv -> {|\Leftrightarrow|}
      | _ -> raise (UnsupportedConnective (c, 2))
      end in
    Printf.sprintf {|(%s %s %s)|} f1 conn f2

  | FConn (c, [f1]) ->
    let f1 = latex_of_form f1 in
    let conn =
      begin match c with
      | `Not -> {|\neg|}
      | _ -> raise (UnsupportedConnective (c, 1))
      end in
    Printf.sprintf {|%s %s|} conn f1

  | FConn (c, fs) ->
      raise (UnsupportedConnective (c, List.length fs))

  | FBind _ ->
      raise UnsupportedFOL

let latex_of_tautos tautos : string =
  let body =
    tautos |> List.fold_left begin fun acc (title, fs) ->
      let equations =
        fs |> List.fold_left begin fun acc f ->
          Printf.sprintf
{|%s\\
%s|} (latex_of_form f) acc
        end "" in
      Printf.sprintf
{|\begin{array}{c}
\text{\textsc{%s}}\\[1em]
%s
\end{array}
\and
%s|} title equations acc
    end "" in
  Printf.sprintf
{|\begin{mathpar}
%s
\end{mathpar}|} body

let () =
  let tautos = parse_tautos BatIO.stdin in
  let latex = latex_of_tautos tautos in
  print_endline latex