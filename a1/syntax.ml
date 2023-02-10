type unop =
  | OpNeg

type binop =
  | OpPlus | OpMinus | OpTimes

type expr =
| ENumLit of int
| EUnOp of unop * expr
| EBinOp of expr * binop * expr
