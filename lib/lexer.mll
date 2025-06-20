{
open Parser

let reservedWords = [
	("int", INT);
	("boolean", BOOLEAN);
	("void", VOID);
	("class", CLASS);
	("out", OUT);
]
}

rule main = parse
  [' ' '\t']+ { main lexbuf }
| [' ' '\t' '\r']* '\n' { Lexing.new_line lexbuf; main lexbuf }
| ['0'-'9']+ { INTV (int_of_string (Lexing.lexeme lexbuf)) }
| "+" { PLUS }
| "=" { EQ }
| "{" { LCURLY }
| "}" { RCURLY }
| "(" { LPAREN }
| ")" { RPAREN }
| ";" { SEMI }
| ['a'-'z' 'A'-'Z'] ['a'-'z' '0'-'9' '_' '\'']*
  {
    let id = Lexing.lexeme lexbuf in
    try
      List.assoc id reservedWords
    with _ -> Parser.ID id
  }
| eof { EOF }
