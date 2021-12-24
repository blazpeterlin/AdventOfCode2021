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
    
    let lns = 
        C.readLines inputFile
        |> List.map (
            fun (ln:string) -> 
                ln |> C.splitCh "|" |> fun xs -> (xs[0] |> C.splitCh " ", xs[1] |> C.splitCh " ")
            )
    0

    let resetCtx () = Gs.overrideContext(Dictionary<string,string>())

    let ctx = resetCtx()
    let a,b,c,d,e,f,g = Int "a", Int "b", Int "c", Int "d", Int "e", Int "f", Int "g"
    let xa,xb,xc,xd,xe,xf,xg = Int "xa", Int "xb", Int "xc", Int "xd", Int "xe", Int "xf", Int "xg"
    let v0,v1,v2,v3,v4,v5,v6,v7,v8,v9= Int "v0", Int "v1", Int "v2", Int "v3", Int "v4", Int "v5", Int "v6", Int "v7", Int "v8", Int "v9"

    let rule0 = a+b+c+e+f+g =. v0
    let rule1 = c+f =. v1
    let rule2 = a+c+d+e+g =. v2
    let rule3 = a+c+d+f+g =. v3
    let rule4 = b+c+d+f =. v4
    let rule5 = a+b+d+f+g =. v5
    let rule6 = a+b+d+e+f+g =. v6
    let rule7 = a+c+f =. v7
    let rule8 = a+b+c+d+e+f+g =. v8
    let rule9 = a+b+c+d+f+g =. v9



    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


