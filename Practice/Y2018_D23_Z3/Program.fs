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
open Microsoft.Z3.Function
open System.Numerics

type ENV = T | P
let internal (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
let internal (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)

// type State = { EL:int; }

[<EntryPoint>]
let main argv =    
    let env =P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let lns = 
        C.readLines inputFile
        |> List.map (fun ln -> ln |> C.splitCh "=<,> rpos" |> List.map (fun str -> str|> BigInteger.Parse) |> fun xs -> (xs[0], xs[1], xs[2]),xs[3])

    0
    
    let dist (x0, y0, z0) (x1, y1, z1) =
      abs (x0 - x1) + abs (y0-y1) + abs (z0-z1)

    let getVal (r:Result) =
        match r with 
        | Func(_) -> failwith "Unexpected func"
        | Const(x) -> x

    //  let f (x: Int) = Z3.CreateFunction<Int>("f", IntSort(), x)
    
    //let zabs (x: Int) = 0  ---- x
    // Z3.CreateFunction<Int>("zabs", IntSort(), If(x <. 0, 0-x, x) )
    
    let opt = Gs.context().MkOptimize()
    
    ////let dist_from_zero = Func


    //let finalOpt = (opt.MkMaximize rangeCount).MkMinimize dist_from_zero

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


