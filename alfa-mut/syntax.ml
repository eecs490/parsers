module Identifier = struct
    type t = string
end

module Loc = struct
    type t = int
end

module Typ = struct
    type t =
        | Num
        | Bool
        | Arrow of t * t
        | Prod of t * t
        | Unit
        | Sum of t * t
        | Ref of t
end

module Exp = struct
    type unop =
        | OpDeref
        | OpNeg

    type binop =
        | OpAssign
        | OpAp
        | OpLt
        | OpGt
        | OpEq
        | OpPlus
        | OpMinus
        | OpTimes

    type t =
        | EVar of Identifier.t
        | ENumLit of int
        | EBoolLit of bool
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
        | EAlloc of t
        | ELoc of Loc.t
end
