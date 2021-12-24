open System
open Aoc21_Common
open Aoc21_Common.Operators
open FParsec
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Security.Cryptography
open System.Text
open Microsoft.Z3
open Microsoft.Z3.Bool
open Microsoft.Z3.Int
open Microsoft.Z3.Real
open Microsoft.Z3.Array
open Microsoft.Z3.Function
open Microsoft.Z3.Api
open Microsoft.Z3.Addons

type ENV = T | P

//type State = { Node:string; }

[<EntryPoint>]
let main argv =    
    let env = P
    
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"

    let parse file = 
        C.readLines file
        |> List.map (fun ln -> 
            ln 
        )
    let lns = parse inputFile
    0
    


    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


