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

%start <Syntax.expr> main
%{ open Syntax %}

%%

main:
| e = expr EOL
    { e }

expr:
| e = boolean
    { e }
| LET v = var OFTYPE t = ty BE e1 = expr IN e2 = expr
    { ELetAnn (v, t, e1, e2) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { EIf (e1, e2, e3) }
| FUN LPAREN v = var OFTYPE t = ty RPAREN ARROW e = expr
    { EFun (v, ty, e) }

boolean:
| e = arith
    { e }
| e1 = arith GT e2 = arith
    { EBinOp (e1, OpGt, e2) }
| e1 = arith LT e2 = arith
    { EBinOp (e1, OpLt, e2) }
| e1 = arith EQ e2 = arith
    { EBinOp (e1, OpEq, e2) }

arith:
| e = factor
    { e }
| e1 = arith PLUS e2 = factor
    { EBinOp (e1, OpPlus,e2) }
| e1 = arith MINUS e2 = factor
    { EBinOp (e1, OpMinus, e2) }

factor:
| e = app
    { e }
| e1 = factor TIMES e2 = app
    { EBinOp (e1, OpTimes, e2) }

app:
| e = simple
    { e }
| e1 = app e2 = simple
    { EBinOp (e1, OpAp, e2) }
| MINUS e = simple
    { EUnOp (OpNeg, e) }

simple:
| e = var
    { EVar (e) }
| i = INT
    { ENumLiteral i }
| b = BOOLLIT
    { EBoolLiteral b }
| LPAREN e = expr RPAREN
    { e }

var:
| v = VAR
    { v }

ty:
| t = base_ty
    { t }
| t1 = base_ty ARROW t2 = ty
    { Arrow (t1, t2) }

base_ty:
| NUM
    { Num }
| BOOL
    { Bool }