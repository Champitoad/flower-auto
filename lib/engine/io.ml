(* -------------------------------------------------------------------- *)
open Utils

module P = Parser
module L = Lexing

(* -------------------------------------------------------------------- *)
let lexbuf_from_channel = fun name channel ->
  let lexbuf = Lexing.from_channel channel in
    lexbuf.Lexing.lex_curr_p <- {
        Lexing.pos_fname = name;
        Lexing.pos_lnum  = 1;
        Lexing.pos_bol   = 0;
        Lexing.pos_cnum  = 0
      };
    lexbuf

(* -------------------------------------------------------------------- *)
let parserfun_type =
  MenhirLib.Convert.Simplified.traditional2revised P.xtype

(* -------------------------------------------------------------------- *)
let parserfun_expr =
    MenhirLib.Convert.Simplified.traditional2revised P.xexpr

(* -------------------------------------------------------------------- *)
let parserfun_nexpr =
    MenhirLib.Convert.Simplified.traditional2revised P.xnexpr

(* -------------------------------------------------------------------- *)
let parserfun_form =
    MenhirLib.Convert.Simplified.traditional2revised P.xform

(* -------------------------------------------------------------------- *)
let parserfun_goal =
    MenhirLib.Convert.Simplified.traditional2revised P.xgoal

(* -------------------------------------------------------------------- *)
type reader = Lexing.lexbuf Disposable.t

let lexbuf (reader : reader) =
  Disposable.get reader

(* -------------------------------------------------------------------- *)
let from_channel ~name channel =
  let lexbuf = lexbuf_from_channel name channel in
  Disposable.create lexbuf

(* -------------------------------------------------------------------- *)
let from_file filename =
  let channel = open_in filename in

  try
    let lexbuf = lexbuf_from_channel filename channel in
    Disposable.create ~cb:(fun _ -> close_in channel) lexbuf

  with
    | e ->
        (try close_in channel with _ -> ());
        raise e

(* -------------------------------------------------------------------- *)
let from_string data =
  Disposable.create (Lexing.from_string data)

(* -------------------------------------------------------------------- *)
let finalize (reader : reader) =
  Disposable.dispose reader

(* -------------------------------------------------------------------- *)
let lexer (lexbuf : L.lexbuf) =
  let token = Lexer.main lexbuf in
  (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

(* -------------------------------------------------------------------- *)
let parse_type (reader : reader) =
  parserfun_type (fun () -> lexer (lexbuf reader))

(* -------------------------------------------------------------------- *)
let parse_expr (reader : reader) =
  parserfun_expr (fun () -> lexer (lexbuf reader))

(* -------------------------------------------------------------------- *)
let parse_nexpr (reader : reader) =
  parserfun_nexpr (fun () -> lexer (lexbuf reader))

(* -------------------------------------------------------------------- *)
let parse_form (reader : reader) =
  parserfun_form (fun () -> lexer (lexbuf reader))

(* -------------------------------------------------------------------- *)
let parse_goal (reader : reader) =
  parserfun_goal (fun () -> lexer (lexbuf reader))
