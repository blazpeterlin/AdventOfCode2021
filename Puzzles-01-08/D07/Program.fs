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
    let env =P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable lns = 
        C.readLines inputFile
        |> List.map (fun ln -> ln |> C.splitCh "," |> List.map int64)
        |> List.concat

    let mn = lns |> List.min
    let mx = lns |> List.max

    let cost a b =
        abs(a-b)*(abs(a-b)+1L)/2L
       
       // 1, 3, 6, 10, 15
       // 

    let (pos1,res1) = [mn..mx] |> List.map (fun i -> (i, lns |> List.map(fun x -> abs(x-i)) |> List.sum)) |> List.minBy snd
    
    let (pos2,res2) = [mn..mx] |> List.map (fun i -> (i, lns |> List.map(fun x -> (cost x i))  |> List.sum)) |> List.minBy snd

    0



    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


