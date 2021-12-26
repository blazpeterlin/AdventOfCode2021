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
open System.Numerics

type ENV = T | P



[<EntryPoint>]
let main argv =    
    let env = T
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let lns = 
        C.readLines inputFile
        |> List.map (fun ln -> ln |> C.splitCh "=<,> rpos" |> List.map (fun str -> str|> BigInteger.Parse |> IntVal) |> fun xs -> (xs[0], xs[1], xs[2]),xs[3])

    0

    let opt = Opt()
    
    let varX = Int "x"
    let varY = Int "y"
    let varZ = Int "z"

    let zAbs (a: Int) = a >=. 0I ??> a --> (0I-a)
    let zDist (a: Int, b: Int) = zAbs(a - b)
    
    let zDrones = [| 
            for (x,y,z),r in lns do
                let zIsInRange = zDist(x,varX) + zDist(y,varY) + zDist(z,varZ) <=. r   ??>   IntVal 1I   -->   IntVal 0I

                yield zIsInRange
        |]

    let zDronesInRangeSum = ArrayVal1D zDrones |> Array1D.SUM
    
    opt.Maximize(zDronesInRangeSum)
    opt.Minimize(varX + varY + varZ)
    opt.CheckOrFail()
    let resDistFromZero = opt.Eval(varX + varY + varZ)

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
