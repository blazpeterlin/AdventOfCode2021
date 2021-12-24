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

type OptimizeFor = MINIMUM | MAXIMUM

[<EntryPoint>]
let main argv =    
    let env = P
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

        let opt = Opt()
        opt.Add(varZ =. 0I)

        let addLn ((inp::inptail):Int list,z:Int) (n1::n2::n3::[] : int list) =
            let ez = z |> asIntExpr

            let resZ = 
                if n2 <= 9
                then // x = 0
                    opt.Add(z%26I + (bigint n2) =. inp)
                    let nextZ = z/(bigint n1)
                    nextZ
                else // x = 1
                    opt.Add(z%26I + (bigint n2) <>. inp)
                    let nextZ = z/(bigint n1) * 26I + inp + (bigint n3)
                    nextZ
         
            inptail, resZ

        let (finalInp,finalZ) = lns |> List.fold addLn (varInputs,varZ)

        opt.Add(finalZ =. 0I)
    
        for inp in varInputs do
            opt.Add(inp >=. 1I)
            opt.Add(inp <=. 9I)

        let totalInputExpr = 
            varInputs |> List.fold (fun acc inp -> (acc * 10I) + inp) (IntVal(0I))

        match optimizeFor with
        | MINIMUM -> opt.Minimize(totalInputExpr)
        | MAXIMUM -> opt.Maximize(totalInputExpr)

        opt.CheckOrFail()
        opt.Eval totalInputExpr |> string |> int64

        
    // 99999795919456 
    let res1 = solveFor MAXIMUM
    // 45311191516111
    let res2 = solveFor MINIMUM

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


