%token <int> INT
%token PLUS MINUS TIMES
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS        /* lowest precedence */
%left TIMES             /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <Syntax.expr> main
%{ open Syntax %}

%%

main:
| e = expr EOL
    { e }

expr:
| i = INT
    { ENumLiteral i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { EBinOp (e1, OpPlus,e2) }
| e1 = expr MINUS e2 = expr
    { EBinOp (e1, OpMinus, e2) }
| e1 = expr TIMES e2 = expr
    { EBinOp (e1, OpTimes, e2) }
| MINUS e = expr %prec UMINUS
    { EUnOp(OpNeg, e) }

