open System
open Aoc21_Common
open Aoc21_Common.Operators
//open FParsec
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Security.Cryptography
open System.Text
open AngouriMath.FSharp.Core
open AngouriMath.FSharp.Functions
open Microsoft.Z3
open Microsoft.Z3.Bool
open Microsoft.Z3.Int
open Microsoft.Z3.Real
open Microsoft.Z3.Array
open Microsoft.Z3.Addons

type ENV = T | P

// type State = { EL:int; }

[<EntryPoint>]
let main argv =    
    let env = T
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let lns = 
        C.readLines inputFile
        |> List.head
        |> fun ln -> ln |> C.splitCh "," |> List.map int

    0

    // part 1
    let abs (a:Int) (b:Int) = b >. a ??> b-a --> (a-b)
    let cost1 (a:Int) (b:Int) = abs a b

    let opt = Opt()
    let r1 = Int "r1"

    let minr = lns |> List.min |> bigint |> IntVal
    let maxr = lns |> List.max |> bigint |> IntVal

    opt.Add (r1 >=. minr)
    opt.Add (r1 <=. maxr)

    let totalCost1 = lns |> List.map (fun num -> cost1 r1 (IntVal num)) |> Array.ofList |> Array1D |> Array1D.SUM

    // cannot handle full input well :(
    opt.Minimize totalCost1
    opt.CheckOrFail()
    let res1 = opt.Eval totalCost1

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


