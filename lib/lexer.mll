{
open Parser

let reservedWords = [
	("int", INT);
	("boolean", BOOLEAN);
	("void", VOID);
	("class", CLASS);
	("out", OUT);
  ("true", TRUE);
  ("false", FALSE);
  ("if", IF);
  ("else", ELSE);
  ("while", WHILE);
  ("return", RETURN);
]
}

rule main = parse
  [' ' '\t']+ { main lexbuf }
| [' ' '\t' '\r']* '\n' { Lexing.new_line lexbuf; main lexbuf }
| ['0'-'9']+ { INTV (int_of_string (Lexing.lexeme lexbuf)) }
| "+" { PLUS }
| "-" { MINUS }
| "*" { PROD }
| "/" { DIV }
| "%" { MOD }
| "<" { LT }
| ">" { GT }
| "<=" { LE }
| ">=" { GE }
| "==" { EQ }
| "!=" { NEQ }
| "&&" { LAND }
| "||" { LOR }
| "=" { SUBSTITUTE }
| "{" { LCURLY }
| "}" { RCURLY }
| "(" { LPAREN }
| ")" { RPAREN }
| ";" { SEMI }
| "," { COMMA }
| ['a'-'z' 'A'-'Z'] ['a'-'z' '0'-'9' '_' '\'']*
  {
    let id = Lexing.lexeme lexbuf in
    try
      List.assoc id reservedWords
    with _ -> Parser.ID id
  }
| eof { EOF }
