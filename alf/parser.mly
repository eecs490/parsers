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

%start <Syntax.Exp.t> main
%{ open Syntax %}

%%

main:
| e = expr EOL
    { e }

expr:
| e = opseq
    { e }
| LET v = var OFTYPE t = ty BE e1 = expr IN e2 = expr
    { Exp.ELetAnn (v, t, e1, e2) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { Exp.EIf (e1, e2, e3) }
| FUN LPAREN v = var OFTYPE t = ty RPAREN ARROW e = expr
    { Exp.EFun (v, t, e) }

opseq:
| e = app
    { e }
| e1 = opseq GT e2 = opseq %prec GT
    { Exp.EBinOp (e1, OpGt, e2) }
| e1 = opseq LT e2 = opseq %prec LT
    { Exp.EBinOp (e1, OpLt, e2) }
| e1 = opseq EQ e2 = opseq %prec EQ
    { Exp.EBinOp (e1, OpEq, e2) }
| e1 = opseq PLUS e2 = opseq %prec PLUS
    { Exp.EBinOp (e1, OpPlus,e2) }
| e1 = opseq MINUS e2 = opseq %prec MINUS
    { Exp.EBinOp (e1, OpMinus, e2) }
| e1 = opseq TIMES e2 = opseq %prec TIMES
    { Exp.EBinOp (e1, OpTimes, e2) }
// | MINUS e = simple /*%prec UMINUS*/
//     { Exp.EUnOp (OpNeg, e) }

app:
| e = simple
    { e }
| e1 = app e2 = simple
    { Exp.EBinOp (e1, OpAp, e2) }

simple:
| i = INT
    { Exp.ENumLit i }
| MINUS i = INT
    { Exp.ENumLit (-i) }
| MINUS e = nonnum
    { Exp.EUnOp (OpNeg, e) }

nonnum:
| e = var
    { Exp.EVar (e) }
| b = BOOLLIT
    { Exp.EBoolLit b }
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