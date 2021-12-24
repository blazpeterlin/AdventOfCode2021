module D02_19

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

let day = 02

// type State = { EL:int; }


let main =    
    let env = P
    let inputFile = basefunc.loadFile day env

    // C.toPosMap (fun (pos) -> "")
    
    let lns = 
        C.readLines inputFile
        |> List.map (
            fun (ln:string) -> 
                ln
            )

    0



    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


