module Parser

open Token
open Ast
open Error

let rec expr tokens =
    let left, tokens = term tokens
    tail left tokens

and tail left tokens =
    match tokens |> Seq.map (fun tok -> tok.kind) with
    | Token.Plus :: rest ->
        let right, rest' = term rest
        tail (left + right) rest'

let parse (tokens: Token list): Result<Expr, Error> =
