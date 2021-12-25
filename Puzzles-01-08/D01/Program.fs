open System
open Aoc21_Common
open Aoc21_Common.Operators
open FParsec
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Security.Cryptography
open System.Text

type ENV = T | P

// type State = { EL:int; }

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable lns = 
        C.readLines inputFile
        //|> Seq.head
        //|> List.map (fun x -> x |> C.splitCh "[]")
        //|> C.parseList id
        //|> C.parseMatrix id
    0

    let res1 =
        lns
        |> List.pairwise
        |> List.filter (fun (x,y) ->(int y) >(int x))
        |> List.length
        
    let res2 =
        lns
        |> List.windowed 3
        |> List.map (fun (x::y::z::[]) ->(int x)+(int y)+(int z))
        |> List.pairwise
        |> List.filter (fun (x,y) ->(int y) >(int x))
        |> List.length
            

    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
