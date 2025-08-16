module Token

open Ast

type TokenKind =
    | Number of float

    | Plus
    | Minus
    | Star
    | Slash

    | LParen
    | RParen

    | Eof

type Token =
    {
        kind: TokenKind
        pos: int
    }

let lexeme = function
    | Number n -> string n

    | Plus -> "+"
    | Minus -> "-"
    | Star -> "*"
    | Slash -> "/"

    | LParen -> "("
    | RParen -> ")"

    | Eof -> ""

let kindToBinOp = function
    | Plus -> BinaryOp.Plus
    | Minus -> BinaryOp.Minus
    | Star -> Times
    | Slash -> Divide
    | kind -> failwithf "Internal - Invalid kind: %A" kind

let kindToUnaryOp = function
    | Minus -> Negate
    | kind -> failwithf "Internal - Invalid kind: %A" kind
