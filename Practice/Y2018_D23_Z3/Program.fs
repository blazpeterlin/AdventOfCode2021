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
open System.Numerics

type ENV = T | P
let internal (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
let internal (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)


let internal (??>) (b:Bool) (expr1:Theory,expr2:Theory) = 
    match expr1,expr2 with
    | (:? Bool as b1),(:? Bool as b2) -> createITE (b |> asBoolExpr) (b1 |> asBoolExpr) (b2 |> asBoolExpr)
    | (:? Int as i1),(:? Int as i2) -> createITE (b |> asBoolExpr) (i1 |> asIntExpr) (i2 |> asIntExpr)
    | (:? Real as r1),(:? Real as r2) -> createITE (b |> asBoolExpr) (r1 |> asRealExpr) (r2 |> asRealExpr)
    | _ ->failwith "Failed to match types in ?>"
let internal (-->) (expr1) (expr2) = (expr1,expr2)


[<EntryPoint>]
let main argv =    
    let env = T
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let lns = 
        C.readLines inputFile
        |> List.map (fun ln -> ln |> C.splitCh "=<,> rpos" |> List.map (fun str -> str|> BigInteger.Parse |> IntVal) |> fun xs -> (xs[0], xs[1], xs[2]),xs[3])

    0

    let opt = Gs.context().MkOptimize()
    
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

    let zDronesInRangeSum = ArrayVal1D_Int zDrones |> Array1D.Z3SUM

    opt.MkMaximize(zDronesInRangeSum)
    opt.MkMinimize(varX + varY + varZ |> asIntExpr)
    
    opt.Check() |> function | Status.UNSATISFIABLE -> failwith "unsatisfiable" | Status.UNKNOWN -> failwith "unknown" | Status.SATISFIABLE -> () |> ignore | huh -> failwith "huh"
    let resModel = opt.Model

    let resDistFromZero = resModel.Eval(varX + varY + varZ |> asIntExpr)

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code




    
    //let ZERO = IntVal 0I
    //let ONE = IntVal 1I



    //dict["model"] <- "true"
    //let opt = Gs.context().MkOptimize()
    
    //let intSort = ctx.MkIntSort() :> Sort
    //let boolSort = ctx.MkBoolSort() :> Sort
    //let realSort = ctx.MkRealSort() :> Sort

    //// works
    //let rule1d = varX >=. IntVal 10I ??> IntVal 1I --> IntVal 0I

    //// works
    //let rule1e = zDist(IntVal 10I, varX) + zDist(IntVal 12I,varY) + zDist(IntVal 12I, varZ) <=. IntVal 2I ??> IntVal 1I --> IntVal 0I

    //opt.MkMaximize(rule1e |> asIntExpr)
    //opt.MkMaximize(varX |> asIntExpr)
    //let status = opt.Check()
    //let resModel = opt.Model

    //// -8885, -2437, 0, 2
    //let x,y,z,c = resModel.Eval(varX |> asIntExpr),resModel.Eval(varY |> asIntExpr),resModel.Eval(varZ |> asIntExpr),resModel.Eval(rule1e |> asIntExpr)
    //let sumCheck = resModel.Eval ( zDist(IntVal 10I, varX) + zDist(IntVal 12I,varY) + zDist(IntVal 12I, varZ) |> asIntExpr)
    //let xcheck,ycheck,zcheck = resModel.Eval(zDist(IntVal 10I, varX) |> asIntExpr), resModel.Eval(zDist(IntVal 12I,varY) |> asIntExpr) , resModel.Eval(zDist(IntVal 12I, varZ) |> asIntExpr)

    
    // works
    //let zDronesInRangeSum = Gs.context().MkAdd(zDrones |> Array.map (asIntExpr >> (fun x -> x :> ArithExpr))) :?> IntExpr |> IntExpr
    // works
