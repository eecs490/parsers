module Typ = struct
  type t = Num | Bool | Arrow of t * t
end

module Identifier = struct
  type t = string

  (* Use this to compare Identifiers for equality! *)
  let equal = String.equal
end

module Expr = struct
  type unop = OpNeg

  type binop = OpAp | OpLt | OpGt | OpEq | OpPlus | OpMinus | OpTimes

  type t =
    | EVar of Identifier.t
    | ENumLiteral of int
    | EBoolLiteral of bool
    | EUnOp of unop * t
    | EBinOp of t * binop * t
    | EIf of t * t * t
    | EFun of Identifier.t * Typ.t * t
    | ELetAnn of Identifier.t * Typ.t * t * t
end

module Value = struct
  type t =
    | VNumLiteral of int
    | VBoolLiteral of bool
    | VFun of Identifier.t * Typ.t * Expr.t

  let to_expr (v:t):Expr.t =
      match v with
      | VNumLiteral n -> ENumLiteral n
      | VBoolLiteral b -> EBoolLiteral b
      | VFun (i, t, e) -> EFun (i, t, e)
end