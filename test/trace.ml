open Prover
open Flower
open Utils

type cycle = (garden * garden * garden)
type trace = cycle list

let lifecycle_trace (t : gtree) : cycle =
  pollination t;
  let p = deepcopy_gtree t in
  reproduction t;
  let r = deepcopy_gtree t in
  decomposition t;
  let d = deepcopy_gtree t in
  (gtree_to_garden p, gtree_to_garden r, gtree_to_garden d)

let life_trace (g : garden) : garden * trace =
  let t = ref (garden_to_gtree g) in
  let t' = ref (deepcopy_gtree !t) in
  let trace = ref [| lifecycle_trace !t |] in
  while not (eq_gtree !t !t') do
    t' := deepcopy_gtree !t;
    trace := Array.append !trace [| lifecycle_trace !t |];
  done;
  gtree_to_garden !t,
  !trace |> Array.(remove_at (length !trace - 1)) |> Array.to_list

let string_of_trace (init : garden) (trace : trace) : string =
  List.fold_left begin fun acc (p, r, d) ->
    let p = string_of_garden p in
    let r = string_of_garden r in
    let d = string_of_garden d in
    Printf.sprintf "⊢ %s\n⊢ %s\n⊢ %s\n%s\n" d r p acc
  end (string_of_garden init) trace

let latex_prftree_of_trace (init : garden) (trace : trace) : string =
  List.fold_left begin fun acc (p, r, d) ->
    let p = latex_of_garden p in
    let r = latex_of_garden r in
    let d = latex_of_garden d in
    Printf.sprintf
{|\R[\mathtt{P}]
  {\R[\mathtt{R}]
  {\R[\mathtt{D}]
  {%s}
  {%s}}
  {%s}}
  {%s}|} d r p acc
  end (latex_of_garden init) trace

let latex_array_of_trace (init : garden) (trace : trace) : string =
  let content =
    List.fold_right begin fun (p, r, d) acc ->
      let p = latex_of_garden p in
      let r = latex_of_garden r in
      let d = latex_of_garden d in
      Printf.sprintf
{|
  \\[1em]\hline\\
  \xstep{\mathtt{P}} &%s\\
  \xstep{\mathtt{R}} &%s\\
  \xstep{\mathtt{D}} &%s%s|} p r d acc
    end trace ""
  in
  Printf.sprintf
{|\begin{array}{rl}
  &%s%s
  \end{array}|} (latex_of_garden init) content

let latex_of_trace = latex_array_of_trace

let () =
  Printexc.record_backtrace true;
  let goals = Tests.parse_tautos BatIO.stdin |> List.map Flower.of_form in
  let traces = goals |> List.map (fun g -> g, life_trace g |> snd) in
  let string_traces = traces |>
    List.map (uncurry string_of_trace) |>
    List.to_string ~sep:"" ~left:"" ~right:"" identity
  in
  let latex_traces = traces |>
    List.map begin fun (init, trace) ->
      latex_of_trace init trace
    end |>
    List.to_string ~sep:"\n  \\and\n  " ~left:"" ~right:"" identity |> fun traces ->
    Printf.sprintf
{|\begin{mathpar}
  %s
\end{mathpar}|} traces
  in
  Printf.printf "%s%s" string_traces latex_traces