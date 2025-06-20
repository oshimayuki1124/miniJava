%{
open Syntax
%}

%token CLASS LCURLY RCURLY LPAREN RPAREN SEMI OUT PLUS EQ INT BOOLEAN VOID EOF

%token<int> INTV
%token<Syntax.id> ID

%start toplevel
%type<Syntax.program> toplevel

%left PLUS

%%

toplevel :
  | cs=list(ClassExpr) EOF { cs }

ClassExpr :
  | CLASS n=ID LCURLY ms=list(MethodExpr) RCURLY { {name=n; methods=ms} }

MethodExpr :
  | TyExpr n=ID LPAREN RPAREN LCURLY es=list(CommandExpr) RCURLY { {name=n; body=es} }

CommandExpr :
  | t=TyExpr id=ID SEMI { Declare (t, id) }
  | id=ID EQ e=Expr SEMI { Substitute (id, e) }
  | e=Expr SEMI { Exp e }

Expr :
  | OUT LPAREN e=Expr RPAREN { Out e }
  | e1=Expr op=Op e2=Expr { BinOp (op, e1, e2) }
  | i=INTV { IConst i }
  | id=ID { Var id }

%inline Op :
  | PLUS { Plus }

TyExpr :
  | INT { TyInt }
  | BOOLEAN { TyBool }
  | VOID { TyVoid }