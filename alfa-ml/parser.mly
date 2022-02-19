%token NUM
%token BOOL
%token ARROW
%token UNIT
%token <int> INT
%token REC IS
%token FORALL
%token <string> EID
%token <string> TID
%token <bool> BOOLLIT
%token IF THEN ELSE
%token FUN LET OFTYPE BE IN
%token FIX COMMA
%token L R LPRJ RPRJ
%token CASE OF
%token PLUS MINUS TIMES
%token GT LT EQ
%token LPAREN RPAREN
%token ROLL UNROLL TYPFUN AT
%token LSQUARE RSQUARE
%token EOL

%left GT LT EQ
%left PLUS MINUS
%left TIMES

%right ARROW
%right SUM
%right PROD

%start <Syntax.Expr.t> main
%{ open Syntax %}

%%

main:
| e = expr EOL
    { e }

expr:
| e = opseq
    { e }
| LET i = EID OFTYPE t = ty BE e1 = expr IN e2 = expr
    { Expr.ELet (i, Some(t), e1, e2) }
| LET i = EID BE e1 = expr IN e2 = expr
    { Expr.ELet (i, None, e1, e2) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { Expr.EIf (e1, e2, e3) }
| FUN LPAREN i = EID OFTYPE t = ty RPAREN ARROW e = expr
    { Expr.EFun (i, Some(t), e) }
| FUN i = EID ARROW e = expr
    { Expr.EFun (i, None, e) }
| FIX LPAREN i = EID OFTYPE t = ty RPAREN ARROW e = expr
    { Expr.EFix (i, Some(t), e) }
| FIX i = EID ARROW e = expr
    { Expr.EFix (i, None, e) }
| L e = expr
    { Expr.EInjL (e) }
| R e = expr
    { Expr.EInjR (e) }
| CASE e1 = expr OF L LPAREN x = EID RPAREN ARROW e2 = expr ELSE R LPAREN y = EID RPAREN ARROW e3 = expr
    { Expr.ECase (e1, x, e2, y, e3) }
| LET LPAREN x = EID COMMA y = EID RPAREN BE e1 = expr IN e2 = expr
    { Expr.ELetPair (x, y, e1, e2) }
| ROLL LPAREN e = expr RPAREN
    { Expr.ERoll (e) }
| UNROLL LPAREN e = expr RPAREN
    { Expr.EUnroll (e) }
| TYPFUN t = TID ARROW e = expr
    { Expr.ETypFun (t, e) }

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

app:
| e = right
    { e }
| e1 = app e2 = right
    { Expr.EBinOp (e1, OpAp, e2) }
| MINUS e = right
    { Expr.EUnOp (OpNeg, e) }

right:
| e = simple
    { e }
| e = right LPRJ
    { Expr.EPrjL (e) }
| e = right RPRJ
    { Expr.EPrjR (e) }
| e = right LSQUARE AT t = ty RSQUARE
    { Expr.ETypAp (e, t) }

simple:
| i = EID
    { Expr.EVar i }
| i = INT
    { Expr.ENumLiteral i }
| b = BOOLLIT
    { Expr.EBoolLiteral b }
| LPAREN e = expr RPAREN
    { e }
| LPAREN RPAREN
    { Expr.ETriv }
| LPAREN e1 = expr COMMA e2 = expr RPAREN
    { Expr.EPair (e1, e2) }

ty:
| t = topseq
    { t }
| REC i = TID IS t = ty
    { Typ.TRec (i, t) }
| FORALL i = TID ARROW t = ty
    { Typ.TForall (i, t) }

topseq:
| t = tsimple
    { t }
| t1 = topseq ARROW t2 = topseq %prec ARROW
    { Typ.TArrow (t1, t2) }
| t1 = topseq PLUS t2 = topseq %prec SUM
    { Typ.TSum (t1, t2) }
| t1 = topseq TIMES t2 = topseq %prec PROD
    { Typ.TProd (t1, t2) }

tsimple:
| i = TID
    { Typ.TVar i }
| NUM
    { Typ.TNum }
| BOOL
    { Typ.TBool }
| UNIT
    { Typ.TUnit }
| LPAREN t = ty RPAREN
    { t }
