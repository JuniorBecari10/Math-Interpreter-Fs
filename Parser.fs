module Parser

open Token
open Ast
open Error
open Computation

let rec expr (tokens: Token list) (i: int): Result<Expr, Error> * int =
    term tokens i

and term tokens i =
    result {
        let! left, nextI = factor tokens i
        
        return
            match tokens.[nextI].kind with
            | Star | Slash as op ->
                result {
                    let right, nextI = term tokens (nextI + 1)
                    let a = Ok (Binary left, right, kindToBinOp op), nextI
                    
                    return! ()
                }

            | _ -> Ok left, nextI
    }

and factor tokens i =
    if i < tokens.Length then
        let make kind =
            {
                kind = kind
                pos = tokens.[i].pos
            }

        match tokens.[i].kind with
        | TokenKind.Number n -> Ok (make (Number n)), i + 1
        | LParen -> group tokens (i + 1)
        | _ -> Error (error (sprintf "Expected expression, but got '%s'" (lexeme tokens.[i].kind))), i
    else
        Error (error "Expected expression, but got end."), i

and group tokens i =
    result {
        let inner, nextI = expr tokens i
        
        return
            if i < tokens.Length then
                if tokens.[i].kind = RParen then
                    Ok inner, nextI + 1
                else
                    Error (error (sprintf "Expected ')', but got '%s'." (lexeme tokens.[i].kind)) nextI), nextI
            else
                Error (error "Expected ')', but got end." nextI), nextI
    }

let parse (tokens: Token list): Result<Expr, Error> =
