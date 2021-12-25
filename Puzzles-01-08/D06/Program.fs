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
        |> List.map (fun ln -> ln |>C.splitCh "," |> List.map int64)
        |> List.concat

    0
        
    let target = if env=T then 256 else 256

    let numFishes0 = [0..8] |> Seq.map (fun x -> (lns |> Seq.filter( fun y -> y=x) |> Seq.length |> int64)) |> List.ofSeq

    let step (r1 : int64 list) _ =
        let r2 = [0L;0L;0L;0L;0L;0L;r1[0];0L;r1[0]]

        let r = List.zip (List.skip 1 (r1@[0L])) r2 |> List.map (fun(x,y) -> x+y)
        r
    
    let res1 = [1..80] |> Seq.fold step numFishes0 |> List.sum

    let res2 = [1..256] |> Seq.fold step numFishes0 |> List.sum

    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


