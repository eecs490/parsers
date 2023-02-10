%token NUM
%token BOOL
%token ARROW
%token UNIT
%token <int> INT
%token <string> ID
%token <bool> BOOLLIT
%token IF THEN ELSE
%token FUN LET OFTYPE BE IN
%token COMMA
%token L R LPRJ RPRJ
%token CASE OF
%token PLUS MINUS TIMES
%token GT LT EQ
%token LPAREN RPAREN
%token EOL

%start <Syntax.Exp.t> main
%{ open Syntax %}

%%

main:
| e = expr EOL
    { e }

expr:
| e = boolean
    { e }
| LET i = id OFTYPE t = ty BE e1 = expr IN e2 = expr
    { Exp.ELet (i, Some(t), e1, e2) }
| LET i = id BE e1 = expr IN e2 = expr
    { Exp.ELet (i, None, e1, e2) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { Exp.EIf (e1, e2, e3) }
| FUN LPAREN i = id OFTYPE t = ty RPAREN ARROW e = expr
    { Exp.EFun (i, Some(t), e) }
| FUN i = id ARROW e = expr
    { Exp.EFun (i, None, e) }
| L e = expr
    { Exp.EInjL (e) }
| R e = expr
    { Exp.EInjR (e) }
| CASE e1 = expr OF L LPAREN x = id RPAREN ARROW e2 = expr ELSE R LPAREN y = id RPAREN ARROW e3 = expr
    { Exp.ECase (e1, x, e2, y, e3)}
| LET LPAREN x = id COMMA y = id RPAREN BE e1 = expr IN e2 = expr
    { Exp.ELetPair (x, y, e1, e2)}


boolean:
| e = arith
    { e }
| e1 = arith GT e2 = arith
    { Exp.EBinOp (e1, Exp.OpGt, e2) }
| e1 = arith LT e2 = arith
    { Exp.EBinOp (e1, Exp.OpLt, e2) }
| e1 = arith EQ e2 = arith
    { Exp.EBinOp (e1, Exp.OpEq, e2) }

arith:
| e = factor
    { e }
| e1 = arith PLUS e2 = factor
    { Exp.EBinOp (e1, Exp.OpPlus,e2) }
| e1 = arith MINUS e2 = factor
    { Exp.EBinOp (e1, Exp.OpMinus, e2) }

factor:
| e = app
    { e }
| e1 = factor TIMES e2 = app
    { Exp.EBinOp (e1, Exp.OpTimes, e2) }

app:
| e = right
    { e }
| e1 = app e2 = right
    { Exp.EBinOp (e1, OpAp, e2) }
| MINUS e = right
    { Exp.EUnOp (OpNeg, e) }

right:
| e = simple
    { e }
| e = right LPRJ
    { Exp.EPrjL (e) }
| e = right RPRJ
    { Exp.EPrjR (e) }

simple:
| e = id
    { Exp.EVar (e) }
| i = INT
    { Exp.ENumLit i }
| b = BOOLLIT
    { Exp.EBoolLit b }
| LPAREN e = expr RPAREN
    { e }
| LPAREN RPAREN
    { Exp.ETriv }
| LPAREN e1 = expr COMMA e2 = expr RPAREN
    { Exp.EPair (e1, e2) }

id:
| i = ID
    { i }

ty:
| t = ty_sum
    { t }
| t1 = ty_sum ARROW t2 = ty
    { Typ.Arrow (t1, t2) }

ty_sum:
| t = ty_prod
    { t }
| t1 = ty_prod PLUS t2 = ty_sum
    { Typ.Sum (t1, t2) }

ty_prod:
| t = base_ty
    { t }
| t1 = base_ty TIMES t2 = ty_prod
    { Typ.Prod (t1, t2) }

base_ty:
| NUM
    { Typ.Num }
| BOOL
    { Typ.Bool }
| UNIT
    { Typ.Unit }
| LPAREN t = ty RPAREN
    { t }
