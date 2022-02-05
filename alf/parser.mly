%token NUM
%token BOOL
%token ARROW
%token <int> INT
%token <string> VAR
%token <bool> BOOLLIT
%token IF THEN ELSE
%token FUN LET OFTYPE BE IN
%token PLUS MINUS TIMES
%token GT LT EQ
%token LPAREN RPAREN
%token EOL

%start <Syntax.Expr.t> main
%{ open Syntax %}

%%

main:
| e = expr EOL
    { e }

expr:
| e = boolean
    { e }
| LET v = var OFTYPE t = ty BE e1 = expr IN e2 = expr
    { Expr.ELetAnn (v, t, e1, e2) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { Expr.EIf (e1, e2, e3) }
| FUN LPAREN v = var OFTYPE t = ty RPAREN ARROW e = expr
    { Expr.EFun (v, t, e) }

boolean:
| e = arith
    { e }
| e1 = arith GT e2 = arith
    { Expr.EBinOp (e1, OpGt, e2) }
| e1 = arith LT e2 = arith
    { Expr.EBinOp (e1, OpLt, e2) }
| e1 = arith EQ e2 = arith
    { Expr.EBinOp (e1, OpEq, e2) }

arith:
| e = factor
    { e }
| e1 = arith PLUS e2 = factor
    { Expr.EBinOp (e1, OpPlus,e2) }
| e1 = arith MINUS e2 = factor
    { Expr.EBinOp (e1, OpMinus, e2) }

factor:
| e = app
    { e }
| e1 = factor TIMES e2 = app
    { Expr.EBinOp (e1, OpTimes, e2) }

app:
| e = simple
    { e }
| e1 = app e2 = simple
    { Expr.EBinOp (e1, OpAp, e2) }
| MINUS i = INT
    { Expr.ENumLiteral i }
| MINUS e = nonnum
    { Expr.EUnOp (OpNeg, e) }

simple:
| e = nonnum
    { e }
| i = INT
    { Expr.ENumLiteral i }

nonnum:
| e = var
    { Expr.EVar (e) }
| b = BOOLLIT
    { Expr.EBoolLiteral b }
| LPAREN e = expr RPAREN
    { e }

var:
| v = VAR
    { v }

ty:
| t = base_ty
    { t }
| t1 = base_ty ARROW t2 = ty
    { Typ.Arrow (t1, t2) }

base_ty:
| NUM
    { Typ.Num }
| BOOL
    { Typ.Bool }
| LPAREN t = ty RPAREN
    { t }