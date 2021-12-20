open System
open Aoc21_Common
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

type ENV = T | P
let internal (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
let internal (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)
// Z3 stuff
let internal (??>) (b:Bool) (expr1:Theory,expr2:Theory) = 
    match expr1,expr2 with
    | (:? Bool as b1),(:? Bool as b2) -> createITE (b |> asBoolExpr) (b1 |> asBoolExpr) (b2 |> asBoolExpr)
    | (:? Int as i1),(:? Int as i2) -> createITE (b |> asBoolExpr) (i1 |> asIntExpr) (i2 |> asIntExpr)
    | (:? Real as r1),(:? Real as r2) -> createITE (b |> asBoolExpr) (r1 |> asRealExpr) (r2 |> asRealExpr)
    | _ ->failwith "Failed to match types in ?>"
let internal (-->) (expr1) (expr2) = (expr1,expr2)

//type State = { Node:string; }

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
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


