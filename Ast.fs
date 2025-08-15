module Ast

type BinaryOp =
    | Plus
    | Minus
    | Times
    | Divide

type UnaryOp =
    | Negate

type ExprKind =
    | Number of float
    | Binary of Expr * Expr * BinaryOp
    | Unary of Expr * UnaryOp

and Expr =
    {
        kind: ExprKind
        pos: int
    }
