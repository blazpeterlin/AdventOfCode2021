﻿open System
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
        |> Seq.pairwise
        |> Seq.filter (fun (x,y) ->(int y) >(int x))
        |> Seq.length
        
    let res2 =
        lns
        |> List.windowed 3
        |> List.map (fun (x::y::z::[]) ->(int x)+(int y)+(int z))
        |> List.pairwise
        |> Seq.filter (fun (x,y) ->(int y) >(int x))
        |> Seq.length
            

    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
