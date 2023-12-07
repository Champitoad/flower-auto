open Prover
open Utils

let () =
  Printexc.record_backtrace true;
  let tautos = Tests.parse_tautos BatIO.stdin in
  Tests.lifeform ~logpoll:true ~printer:(Some Flower.string_of_node) tautos