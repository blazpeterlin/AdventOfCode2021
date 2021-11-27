open System
open Aoc21_Common
open FParsec
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Security.Cryptography
open System.Text

type ENV = T | P
let internal (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
let internal (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)


// type State = { EL:int; }
type Lex = Str of string | Level of Lex list | Garbage of string | Arr of Lex list

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable lns = 
        C.readLines inputFile
        //|> C.parseList id
        //|> C.parseMatrix id
        |> Seq.toArray

    
    lns <- [| "{{<a!>},{<a!>},{<a!>},{<ab>}}" |]

    let opp = new OperatorPrecedenceParser<Lex,unit,unit>()
    let expr = opp.ExpressionParser
    
    opp.TermParser <-
        sepBy1 (
                between (pstring "<") (pstring ">") (manyChars (noneOf ">")) |>> Garbage
                <|>
                (between (pstring "{") (pstring "}") expr |>> fun x -> [x] |> Level)
                <|>
                (pstring "" |>> Str)
            ) 
            (skipString ",")
        |>> (fun x -> if x.Length = 1 then x.[0] else Arr(x))


    let normalStr = satisfy (fun c -> c <> '!') |>> (fun ch -> new string([|ch|]))
    let escapedStr = pstring "!" >>. skipAnyChar >>. pstring ""

    let removeEscaped = manyStrings (normalStr <|> escapedStr)
    

    let fullExpr = between (pstring "{") (pstring "}") expr |>> (fun x -> Level([x]))

    let parsed x = 
        x 
        |> fun ln -> match run removeEscaped ln with
                        | Failure(errorMsg,_,_) -> Debug.WriteLine(errorMsg);failwith errorMsg
                        | Success(str,_,_) -> str
        |> fun ln -> match run fullExpr ln with 
                        | Failure(errorMsg,_,_) -> Debug.WriteLine(errorMsg);failwith errorMsg
                        | Success(str,_,_) -> str
    
    let parsedLns = 
        lns 
        |> Array.map parsed


    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
