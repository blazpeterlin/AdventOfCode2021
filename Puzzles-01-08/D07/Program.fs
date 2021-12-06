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

[<EntryPoint>]
let main argv =    
    let env =P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable lns = 
        C.readLines inputFile
        |> List.map (fun ln -> ln)

    0



    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


