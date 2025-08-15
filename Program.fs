open System
open Computation

let rec repl () =
    printf "> "

    let input = Console.ReadLine ()
    let res = result {
        let! tokens = Lexer.lex input
        let! ast = Parser.parse tokens

        return ()
    }

    match res with
    | Ok value -> printfn "< %f" value
    | Err error -> ()

    repl ()

[<EntryPoint>]
let main _ = repl ()
