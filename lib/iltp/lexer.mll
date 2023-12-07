(* -------------------------------------------------------------------- *)
{
  open Engine.Utils
  open Parser

  module L = Location

  (* ------------------------------------------------------------------ *)
  let lex_error lexbuf msg =
    raise (Syntax.ParseError (Some (L.of_lexbuf lexbuf), Some msg))

  (* ------------------------------------------------------------------ *)
  let _keywords = [
    "fof", FOF;
  ]

  (* ------------------------------------------------------------------ *)
  let keywords =
    let table = Hashtbl.create 0 in
    List.iter (curry (Hashtbl.add table)) _keywords; table
}

let empty   = ""
let blank   = [' ' '\t' '\r']
let newline = '\n'
let upper   = ['A'-'Z']
let lower   = ['a'-'z']
let letter  = upper | lower
let digit   = ['0'-'9']
let uint    = digit+

let ichar = (letter | digit | '_' | '\'')
let ident = letter ichar*
let nat   = digit+

(* -------------------------------------------------------------------- *)
rule main = parse
  | "%"         { comment lexbuf }
  | newline     { Lexing.new_line lexbuf; main lexbuf }
  | blank+      { main lexbuf }
  | ident as id { try Hashtbl.find keywords id with Not_found -> IDENT id }
  
  | "$true"  { TRUE  }
  | "$false" { FALSE  }

  | "("   { LPAREN    }
  | ")"   { RPAREN    }
  | "&"   { LAND      }
  | "|"   { LOR       }
  | "~"   { LNEG      }
  | "=>"  { LARROW    }
  | "<=>" { LRARROW   }
  | ","   { COMMA     }
  | "."   { DOT       }

  | eof { EOF }

  |  _ as c { lex_error lexbuf (Printf.sprintf "illegal character: %c" c) }

and comment = parse
  | newline   { Lexing.new_line lexbuf; main lexbuf }
  | eof       { EOF }
  | _         { comment lexbuf }
