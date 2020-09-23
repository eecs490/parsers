type ty = Num | Bool | Arrow of ty * ty

type unop = OpNeg

type binop = OpAp | OpLt | OpGt | OpEq | OpPlus | OpMinus | OpTimes

type identifier = string

type expr =
  | EVar of identifier
  | ENumLiteral of int
  | EBoolLiteral of bool
  | EUnOp of unop * expr
  | EBinOp of expr * binop * expr
  | EIf of expr * expr * expr
  | EFun of identifier * ty * expr
  | ELetAnn of identifier * ty * expr * expr
