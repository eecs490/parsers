module Identifier = struct
    type t = string
end

module Typ = struct
  type ('var, 'binding) p =
    | TUnit | TBool | TNum
    | TVar of 'var
    | TRec of 'binding * ('var, 'binding) p
    | TForall of 'binding * ('var, 'binding) p
    | TProd of ('var, 'binding) p * ('var, 'binding) p
    | TSum of ('var, 'binding) p * ('var, 'binding) p
    | TArrow of ('var, 'binding) p * ('var, 'binding) p
  (* types that use particular identifiers *)
  type t = (Identifier.t, Identifier.t) p
end

module Expr = struct
    type unop =
        | OpNeg

    type binop =
        | OpAp
        | OpLt
        | OpGt
        | OpEq
        | OpPlus
        | OpMinus
        | OpTimes

    type t =
        | EVar of Identifier.t
        | ENumLiteral of int
        | EBoolLiteral of bool
        | EUnOp of unop * t
        | EBinOp of t * binop * t
        | EIf of t * t * t
        | EFun of Identifier.t * Typ.t option * t
        | ELet of Identifier.t * Typ.t option * t * t
        | EFix of Identifier.t * Typ.t option * t
        | EPair of t * t
        | ETriv
        | ELetPair of Identifier.t * Identifier.t * t * t
        | EPrjL of t
        | EPrjR of t
        | EInjL of t
        | EInjR of t
        | ECase of t * Identifier.t * t * Identifier.t * t
        | ERoll of t
        | EUnroll of t
        | ETypFun of Identifier.t * t
        | ETypAp of t * Typ.t
end

