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
open Microsoft.Z3.BitVec
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
                |> List.map int64
            )
    0
    let lns = parse inputFile

    // how many bits are needed to represent a 10^14
    let maxnum = pown 10L 14
    let BITS = log (maxnum |> float) / log (2.0) |> ceil |> int
    let zint = BitVec BITS
    let zval = BitVecVal BITS
    0
    
    let solveFor optimizeFor =
        
        let varZ = zint "z0"
        let varInputs = [ for x in 1..lns.Length do yield zint("inp"+x.ToString()) ]

        let opt = Opt()
        opt.Add(varZ =. 0L)

        let addLn ((inp::inptail):BitVec list,z:BitVec) (n1::n2::n3::[] : int64 list) =
            let resZ = 
                let x =  z%26L + n2 =. inp   ??>  zval 0  -->   zval 1
                z/n1 + x * (z/n1 * 25L + inp + n3)
            inptail, resZ

        let (finalInp,finalZ) = lns |> List.fold addLn (varInputs,varZ)

        opt.Add(finalZ =. 0L)
    
        for inp in varInputs do
            opt.Add(inp >=. 1L)
            opt.Add(inp <=. 9L)

        let totalInputExpr = 
            varInputs |> List.fold (fun acc inp -> (acc * 10L) + inp) (zval 0)

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


