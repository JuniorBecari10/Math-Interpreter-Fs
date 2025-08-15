module Lexer

open System
open Token
open Error
open Computation

let private token kind pos =
    {
        kind = kind
        pos = pos
    }

let private peek offset (input: string) (i: int): char option =
    if i + offset < input.Length
        then Some input.[i + offset]
        else None

let rec private lexOne (input: string) (pos: int): Result<Token * int, Error> =
    let token kind = token kind pos
    let error msg = error msg pos

    if pos >= input.Length then
        Ok (token Eof, pos)
    else
        let c = input.[pos]

        let next () = lexOne input (pos + 1)
        let make kind = Ok (token kind, pos + 1)

        match c with
        | _ when Char.IsWhiteSpace c -> next ()

        | '+' -> make Plus
        | '-' -> make Minus
        | '*' -> make Star
        | '/' -> make Slash

        | '(' -> make LParen
        | ')' -> make RParen

        | n when Char.IsDigit n -> number input pos
        | _ -> Error (error (sprintf "Unknown character: '%c'" c))

and private number (input: string) (pos: int): Result<Token * int, Error> =
    let token kind = token kind pos
    let error msg = error msg pos
    let peek offset = peek offset input pos
    
    let sb = Text.StringBuilder ()
    let rec readNum i =
        if i < input.Length && Char.IsDigit input.[i] then
            sb.Append input.[i] |> ignore
            readNum (i + 1)
        else i
    
    let firstPos = readNum pos
    let peekIsDot =
        match peek 0 with
        | Some c -> c = '.'
        | None -> false

    let finalPos =
        if peekIsDot
            then readNum firstPos + 1
            else firstPos
    
    let numError = Error (error "Invalid number.")
    let numStr = input.Substring(pos, finalPos - pos - 1)

    let literal = 
        try Ok (token (Number (float numStr)))
        with _ -> numError

    literal
    |> Result.map (fun tok -> tok, pos + numStr.Length)

let rec private lexAll (input: string) (pos: int): Result<Token list, Error> =
    result {
        let! token, nextPos = lexOne input pos

        if token.kind = Eof then
            return [token]
        else
            let! rest = lexAll input nextPos
            return token :: rest
    }

let lex input = lexAll input 0
