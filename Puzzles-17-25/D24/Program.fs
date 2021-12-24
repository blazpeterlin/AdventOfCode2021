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
open Microsoft.Z3.ArrayZ3
open Microsoft.Z3.Function
open Microsoft.Z3.Api

type ENV = T | P
let internal (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
let internal (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)

//type State = { Node:string; }


// Z3 stuff
let internal (??>) (b:Bool) (expr1:Theory,expr2:Theory) = 
    match expr1,expr2 with
    | (:? Bool as b1),(:? Bool as b2) -> createITE (b |> asBoolExpr) (b1 |> asBoolExpr) (b2 |> asBoolExpr)
    | (:? Int as i1),(:? Int as i2) -> createITE (b |> asBoolExpr) (i1 |> asIntExpr) (i2 |> asIntExpr)
    | (:? Real as r1),(:? Real as r2) -> createITE (b |> asBoolExpr) (r1 |> asRealExpr) (r2 |> asRealExpr)
    | _ ->failwith "Failed to match types in ?>"
let internal (-->) (expr1) (expr2) = (expr1,expr2)

type OptimizeFor = MINIMUM | MAXIMUM

[<EntryPoint>]
let main argv =    
    let env = P
    //let inputFile = env |> function | T -> "test.txt" | P -> "inputV2.txt"

    //let parse file = 
    //    C.readLines file
    //    |> List.map (
    //        fun (ln:string) -> 
    //            ln |> int |> int
    //        )
    //    |> List.chunkBySize 3
    //0
    //let lns = parse inputFile
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"

    let parse file = 
        C.readLines file
        |> List.map (fun ln -> ln |> C.splitCh " ")
        |> List.chunkBySize 18
        |> List.map (
            fun (lns:string list list) -> 
                lns[4][2]::lns[5][2]::lns[15][2]::[]
                |> List.map int
            )
    0
    let lns = parse inputFile
    
    
    let solveFor optimizeFor =
        
        let varZ = Int "z0"
        let varInputs = [ for x in 1..lns.Length do yield Int ("inp"+x.ToString()) ]

        let opt = Gs.context().MkOptimize()
        opt.Add(varZ =. 0I |> asBoolExpr)

        let addLn ((inp::inptail):Int list,z:Int) (n1::n2::n3::[] : int list) =
            let ez = z |> asIntExpr

            let resZ = 
                if n2 <= 9
                then
                    opt.Add(z%26I + (bigint n2) =. inp |> asBoolExpr)
                    // x = 0
                    let nextZ = z/(bigint n1)
                    nextZ
                else
                    opt.Add(z%26I + (bigint n2) <>. inp |> asBoolExpr)
                    // x = 1
                    let nextZ = z/(bigint n1) * 26I + inp + (bigint n3)
                    nextZ
         
            inptail, resZ

        let (finalInp,finalZ) =
            lns
            |> List.fold addLn (varInputs,varZ)

        opt.Add(finalZ =. 0I |> asBoolExpr)
    
        for inp in varInputs do
            opt.Add(inp >=. 1I |> asBoolExpr)
            opt.Add(inp <=. 9I |> asBoolExpr)

        let totalInputExpr = 
            varInputs |> List.fold (fun acc inp -> (acc * 10I) + inp) (IntVal(0I))

        match optimizeFor with
        | MINIMUM -> opt.MkMinimize(totalInputExpr |> asIntExpr)
        | MAXIMUM -> opt.MkMaximize(totalInputExpr |> asIntExpr)
        |> ignore

        opt.Check() |> function | Status.UNSATISFIABLE -> failwith "unsatisfiable" | Status.UNKNOWN -> failwith "unknown" | Status.SATISFIABLE -> () |> ignore | huh -> failwith "huh"
        let resModel = opt.Model

        let res = resModel.Eval(totalInputExpr |> asIntExpr)
        res.ToString() |> int64

        
    // 99999795919456 
    let res1 = solveFor MAXIMUM
    // 45311191516111
    let res2 = solveFor MINIMUM

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


