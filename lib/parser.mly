%{
open Syntax
%}

%token CLASS LCURLY RCURLY LPAREN RPAREN SEMI OUT SUBSTITUTE INT BOOLEAN VOID EOF
%token TRUE FALSE IF ELSE WHILE PLUS MINUS PROD DIV MOD LT GT LE GE EQ NEQ LAND LOR
%token COMMA RETURN

%token<int> INTV
%token<Syntax.id> ID

%start toplevel
%type<Syntax.program> toplevel

%right LOR
%right LAND
%left LT GT LE GE EQ NEQ
%left PLUS MINUS
%left PROD DIV MOD

%%

toplevel :
  | cs=ClassesExpr EOF { cs }

ClassesExpr :
  | c=ClassExpr cs=ClassesExpr { match c with (id, ms) -> Store.add id ms cs }
  | (*empty*) { Store.empty }

ClassExpr :
  | CLASS id=ID LCURLY ms=MethodsExpr RCURLY { (id, ms) }

MethodsExpr :
  | m=MethodExpr ms=MethodsExpr { match m with (id, args, rt, cs) -> Store.add id (args, rt, cs) ms }
  | (*empty*) { Store.empty }

MethodExpr :
  | rt=TyExpr id=ID LPAREN args=separated_list(COMMA, ArgExpr) RPAREN LCURLY cs=list(CommandExpr) RCURLY { (id, args, rt, cs) }

ArgExpr :
  | ty=TyExpr id=ID { (id, ty) }

CommandExpr :
  | t=TyExpr id=ID SEMI { Declare (t, id) }
  | id=ID SUBSTITUTE e=Expr SEMI { Substitute (id, e) }
  | IF LPAREN e=Expr RPAREN LCURLY cs1=list(CommandExpr) RCURLY els=ElseExpr { If ((e, cs1)::els) }
  | WHILE LPAREN e=Expr RPAREN LCURLY cs=list(CommandExpr) RCURLY { While (e, cs) }
  | RETURN e=Expr SEMI { Return e }
  | e=Expr SEMI { Exp e }

ElseExpr :
  | ELSE IF LPAREN e=Expr RPAREN LCURLY cs1=list(CommandExpr) RCURLY els=ElseExpr { (e, cs1)::els }
  | ELSE LCURLY cs=list(CommandExpr) RCURLY { [(BConst true, cs)] }
  | (*empty*) { [] }

Expr :
  | OUT LPAREN e=Expr RPAREN { Out e }
  | id=ID LPAREN args=separated_list(COMMA, Expr) RPAREN { Call (id, args) }
  | e1=Expr op=Op e2=Expr { BinOp (op, e1, e2) }
  | i=INTV { IConst i }
  | TRUE { BConst true }
  | FALSE { BConst false }
  | id=ID { Var id }

%inline Op :
  | PLUS { Plus }
  | MINUS { Minus }
  | PROD { Prod }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | GT { Gt }
  | LE { Le }
  | GE { Ge }
  | EQ { Eq }
  | NEQ { Neq }
  | LAND { Land }
  | LOR { Lor }

TyExpr :
  | INT { TyInt }
  | BOOLEAN { TyBool }
  | VOID { TyVoid }