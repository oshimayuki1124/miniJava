%{
open Syntax
%}

%token CLASS LCURLY RCURLY LPAREN RPAREN SEMI OUT SUBSTITUTE INT BOOLEAN VOID EOF
%token TRUE FALSE IF ELSE WHILE PLUS MINUS PROD DIV MOD LT GT LE GE EQ NEQ LAND LOR
%token COMMA RETURN NEW DOT

%token<int> INTV
%token<Syntax.id> ID CID

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
  | c=ClassExpr cs=ClassesExpr { match c with (id, members) -> Store.add id members cs }
  | (*empty*) { Store.empty }

ClassExpr :
  | CLASS cid=CID LCURLY members=MembersExpr RCURLY { (cid, members) }

MembersExpr :
  | d=TyAndIdExpr SEMI members=MembersExpr { match d, members with (id, ty), (fs, ms) -> Store.add id (ty, initial_val ty) fs, ms }
  | d=TyAndIdExpr LPAREN args=separated_list(COMMA, TyAndIdExpr) RPAREN LCURLY cs=list(CommandExpr) RCURLY members=MembersExpr { 
    match d, members with (id, rt), (fs, ms) -> fs, Store.add id (args, rt, cs) ms
    }
  | (*empty*) { Store.empty, Store.empty }

// MethodsExpr :
//   | m=MethodExpr ms=MethodsExpr { match m with (id, args, rt, cs) -> Store.add id (args, rt, cs) ms }
//   | (*empty*) { Store.empty }

// MethodExpr :
//   | ti=TyAndIdExpr LPAREN args=separated_list(COMMA, TyAndIdExpr) RPAREN LCURLY cs=list(CommandExpr) RCURLY { (id, args, rt, cs) }

TyAndIdExpr :
  | ty=TyExpr id=ID { (id, ty) }

CommandExpr :
  | d=TyAndIdExpr SEMI { match d with (id, ty) -> Declare (ty, id) }
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
  | NEW cid=CID LPAREN RPAREN { Instanciation cid }
  | ids=separated_nonempty_list(DOT, ID) args=option(delimited(LPAREN, separated_list(COMMA, Expr), RPAREN)) { 
    match ids, args with
      | id :: [], None -> Var id
      | _, None -> Access_field ids
      | _, Some args -> Call_method (ids, args) 
    }
  | e1=Expr op=Op e2=Expr { BinOp (op, e1, e2) }
  | i=INTV { IConst i }
  | TRUE { BConst true }
  | FALSE { BConst false }
  // | id=ID { Var id }

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
  | cid=CID { TyObj cid }