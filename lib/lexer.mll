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
  ("new", NEW);
]
}

rule main = parse
  [' ' '\t']+ { main lexbuf }
| [' ' '\t' '\r']* '\n' { Lexing.new_line lexbuf; main lexbuf }
| "//" [^ '\n']* '\n' { Lexing.new_line lexbuf; main lexbuf }
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
| "." { DOT }
| ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
  {
    let id = Lexing.lexeme lexbuf in
    try
      List.assoc id reservedWords
    with _ -> Parser.ID id
  }
| ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
  { 
    let cid = Lexing.lexeme lexbuf in
    try 
      List.assoc cid reservedWords
    with _ -> Parser.CID cid
  }
| eof { EOF }
