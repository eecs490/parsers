module Typ = struct
  type t = Num | Bool | Arrow of t * t
end

module Identifier = struct
  type t = string

  (* Use this to compare Identifiers for equality! *)
  let equal = String.equal
end

module Exp = struct
  type unop = OpNeg

  type binop = OpAp | OpLt | OpGt | OpEq | OpPlus | OpMinus | OpTimes

  type t =
    | EVar of Identifier.t
    | ENumLit of int
    | EBoolLit of bool
    | EUnOp of unop * t
    | EBinOp of t * binop * t
    | EIf of t * t * t
    | EFun of Identifier.t * Typ.t * t
    | ELetAnn of Identifier.t * Typ.t * t * t
end

module Value = struct
  type t =
    | VNumLit of int
    | VBoolLit of bool
    | VFun of Identifier.t * Typ.t * Exp.t

  let to_expr (v:t):Exp.t =
      match v with
      | VNumLit n -> ENumLit n
      | VBoolLit b -> EBoolLit b
      | VFun (i, t, e) -> EFun (i, t, e)
end