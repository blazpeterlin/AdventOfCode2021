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
    0
    let lns = parse inputFile

    let size = 14

    // how many bits are needed to represent a 10^14
    let maxnum = pown 10L size
    let BITS = log (maxnum |> float) / log (2.0) |> ceil |> int
    let zint = BitVec BITS
    let zval = BitVecVal BITS
    0
    
    let solveFor optimizeFor =
        
        let varX::varY::varZ::varW::[] = ["x0";"y0";"z0";"w0"] |> List.map (fun nm -> zint nm)
        let varInputs = [ for x in 1..size do yield zint("inp"+(x.ToString())) ]

        let opt = Opt()
        [varX;varY;varZ;varW] |> List.map (fun v -> v =. 0L) |> List.iter (opt.Add)

        let addLn ((inputs):BitVec list,x::y::z::w::[]:BitVec list) (ins::leftvar::instail : string list) =
            let prmA = match leftvar with | "x" -> x | "y" -> y | "z" -> z | "w" -> w | _ -> failwith "huh"
            let prmB = 
                match instail with
                | [] -> zval 0 // not used
                | ["x"] -> x
                | ["y"] -> y
                | ["z"] -> z
                | ["w"] -> w
                | [strNum] -> zval (int strNum)

            let targetVar, nextInputs = 
                match ins, inputs with
                | "inp", inp::inptail -> inp,inptail
                | "add", inps -> prmA + prmB ,inps
                | "mul", inps -> prmA * prmB ,inps
                | "div", inps -> prmA / prmB ,inps
                | "mod", inps -> prmA % prmB ,inps
                | "eql", inps -> prmA =. prmB ??>  zval 1  -->  zval 0 ,inps
                | _ -> failwith "huh"

            let resultVars = 
                match leftvar with 
                | "x" -> targetVar::y::z::w::[] 
                | "y" -> x::targetVar::z::w::[] 
                | "z" -> x::y::targetVar::w::[]  
                | "w" -> x::y::z::targetVar::[]  
                | _ -> failwith "huh"

            nextInputs, resultVars

        let (finalInp,fx::fy::fz::fw::[]) = lns |> List.fold addLn (varInputs, varX::varY::varZ::varW::[])

        opt.Add(fz =. 0L)
    
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


