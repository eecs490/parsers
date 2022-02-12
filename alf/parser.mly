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

%left GT LT EQ
%left PLUS MINUS
%left TIMES

%start <Syntax.Expr.t> main
%{ open Syntax %}

%%

main:
| e = expr EOL
    { e }

expr:
| e = opseq
    { e }
| LET v = var OFTYPE t = ty BE e1 = expr IN e2 = expr
    { Expr.ELetAnn (v, t, e1, e2) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { Expr.EIf (e1, e2, e3) }
| FUN LPAREN v = var OFTYPE t = ty RPAREN ARROW e = expr
    { Expr.EFun (v, t, e) }

opseq:
| e = app
    { e }
| e1 = opseq GT e2 = opseq %prec GT
    { Expr.EBinOp (e1, OpGt, e2) }
| e1 = opseq LT e2 = opseq %prec LT
    { Expr.EBinOp (e1, OpLt, e2) }
| e1 = opseq EQ e2 = opseq %prec EQ
    { Expr.EBinOp (e1, OpEq, e2) }
| e1 = opseq PLUS e2 = opseq %prec PLUS
    { Expr.EBinOp (e1, OpPlus,e2) }
| e1 = opseq MINUS e2 = opseq %prec MINUS
    { Expr.EBinOp (e1, OpMinus, e2) }
| e1 = opseq TIMES e2 = opseq %prec TIMES
    { Expr.EBinOp (e1, OpTimes, e2) }
// | MINUS e = simple /*%prec UMINUS*/
//     { Expr.EUnOp (OpNeg, e) }

app:
| e = simple
    { e }
| e1 = app e2 = simple
    { Expr.EBinOp (e1, OpAp, e2) }

simple:
| i = INT
    { Expr.ENumLiteral i }
| MINUS i = INT
    { Expr.ENumLiteral (-i) }
| MINUS e = nonnum
    { Expr.EUnOp (OpNeg, e) }

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