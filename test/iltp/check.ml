open Prover
open Utils

let domain_path (domain : string) : string =
  Printf.sprintf "test/iltp/ILTP-v1.1.2-propositional/Problems/%s" domain

let all_domain_paths (domain : string) : string list =
  let path = domain_path domain in
  Sys.readdir path |>
  Array.to_list |>
  List.map begin fun filename ->
    Printf.sprintf "%s/%s" path filename
  end

let get_problem_path (domain : string) (id : string) : string =
  Printf.sprintf "%s/%s%s.p"
    (domain_path domain) domain id

let parse_problem (path : string) : Engine.Fo.form =
  try
    readfile path |>
    Iltp.Io.from_string |>
    Iltp.Io.parse_problem |>
    Iltp.Fo.problem
  with Iltp.Syntax.ParseError (loc, msg) ->
    Printf.printf "Parse error in file \"%s\": %s\n"
      path (msg |> BatOption.default "unknown");
    Printf.printf "Position%s\n"
      (loc |> BatOption.default Iltp.Location._dummy |> Iltp.Location.tostring);
    exit 1

let test_form () =
  let form =
    "( ~(( ( p => q)  & ( t => r)  )) => ( ~(~(p)) & ( s & s ) ))" |>
  Iltp.Io.from_string |>
  Iltp.Io.parse_form |>
  Iltp.Fo.check in
  Printf.printf "%s\n" (Engine.Fo.Notation.f_toascii form)

let () =
  Printexc.record_backtrace true;
  
  let logpoll = Array.mem "--logpoll" Sys.argv in
  
  let paths =
    try
      let i = Array.findi (fun arg -> arg = "--domain") Sys.argv in
      let domain = Array.get Sys.argv (i + 1) in
      try
        let i = Array.findi (fun arg -> arg = "--problem") Sys.argv in
        let id = Array.get Sys.argv (i + 1) in
        [get_problem_path domain id]
      with Not_found ->
        all_domain_paths domain
    with _ ->
      Printf.printf
        "Usage: check --domain <domain> [--problem <problem>]\n";
        exit 0 in

  let tautos = List.map parse_problem paths in

  (* Printf.printf "%s\n\n" (List.to_string ~sep:"\n" ~left:"" ~right:"" Engine.Fo.Notation.f_toascii tautos); *)
  (* Tests.test_life ~printer:(Some Flower.string_of_node) (List.concat_map Flower.of_form tautos) *)
  Tests.lifeform ~logpoll tautos