module D01_19

open System
open Aoc21_Common
open Aoc21_Common.Operators
open FParsec
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Security.Cryptography
open System.Text
open basefunc

let day = 01

// type State = { EL:int; }


let main =    
    let env = P
    let inputFile = basefunc.loadFile day env

    // C.toPosMap (fun (pos) -> "")
    
    let lns = 
        C.readLines inputFile
        |> List.map (
            fun (ln:string) -> 
                ln |> int
            )

    0

    let res1 = 
        lns
        |> List.map (fun x -> x/3-2)
        |> List.sum

    let rec x3t2 num =
        if num=0 then 0 else
        let num2 = num/3-2
        if num2 <= 0 then 0 else
        num2 + (x3t2 num2)

    let res2 = 
        lns
        |> List.map (fun num -> x3t2 num)
        |> List.sum

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


