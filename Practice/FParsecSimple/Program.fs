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
type State = { Tgt:string; IncDec:string; Num:int; Src:string; CompOp:string; CompNum:int }
//type Lex = Str of string | Level of Lex list | Garbage of string | Arr of Lex list

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable lns = 
        C.readLines inputFile
        //|> C.parseList id
        //|> C.parseMatrix id
        |> Seq.toArray
    0
    
    lns <- [| "abc (123) -> def, ghi" |]

    //// FPARSEC
    // "abc (123)"
    let parseLnLEFT arg = many1Chars letter .>> skipString " (" .>>. pint32 .>> skipString ")" <| arg

    // " -> abc, def, ghi"
    let parseLnRIGHT arg = skipString " -> " >>. sepBy1 (many1Chars letter) (skipString ", ") <| arg // (skipMany1 (pchar ' ' <|> pchar ','))
    let parseLn arg = parseLnLEFT .>>. opt parseLnRIGHT <| arg

    // strings separated by space
    let parseLn2 arg = sepBy (many1Chars (noneOf (" \t\n".ToCharArray()))) spaces1 <| arg

    // PARSE string x
    let parsed x = x |> fun ln -> 
          match run parseLn2 ln with 
          | Failure(errorMsg,_,_) -> Debug.WriteLine(errorMsg);failwith errorMsg
          | Success(str,_,_) -> { Tgt=str.[0];IncDec=str.[1];Num=int str.[2];Src=str.[4];CompOp=str.[5];CompNum=int str.[6] }
    let parsedLns = lns |> Array.map parsed



    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
