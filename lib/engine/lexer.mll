(* -------------------------------------------------------------------- *)
{
  open Utils
  open Parser

  module L = Location

  (* ------------------------------------------------------------------ *)
  let lex_error lexbuf msg =
    raise (Syntax.ParseError (Some (L.of_lexbuf lexbuf), Some msg))

  (* ------------------------------------------------------------------ *)
  let _keywords = [
    "true"  , TRUE  ;
    "exists", EXISTS;
    "false" , FALSE ;
    "forall", FORALL;
    "rec"   , REC   ;
    "type"  , TYPE  ;
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
  | newline     { Lexing.new_line lexbuf; main lexbuf }
  | blank+      { main lexbuf }
  | ident as id { try Hashtbl.find keywords id with Not_found -> IDENT id }
  | nat as n    { NAT (int_of_string n) }

  | "("   { LPAREN    }
  | ")"   { RPAREN    }
  | "{"   { LBRACE    }
  | "}"   { RBRACE    }
  | "&&"  { LAND      }
  | "||"  { LOR       }
  | "~"   { LNEG      }
  | "->"  { LARROW    }
  | "<->" { LRARROW   }
  | "|-"  { PROOF     }
  | ","   { COMMA     }
  | "."   { DOT       }
  | ";"   { SEMICOLON }
  | ":"   { COLON     }
  | "::"  { DCOLON    }
  | "="   { EQ        }
  | ":="  { COLONEQ   }
  | "&"   { AMP       }
  | "+"   { PLUS      }
  | "*"   { STAR      }

  | eof { EOF }

  |  _ as c { lex_error lexbuf (Printf.sprintf "illegal character: %c" c) }
