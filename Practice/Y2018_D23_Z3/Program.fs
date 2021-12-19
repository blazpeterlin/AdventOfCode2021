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
open Microsoft.Z3.Api
open System.Numerics

type ENV = T | P
let internal (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
let internal (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)

// type State = { EL:int; }

[<EntryPoint>]
let main argv =    
    let env =T
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let lns = 
        C.readLines inputFile
        |> List.map (fun ln -> ln |> C.splitCh "=<,> rpos" |> List.map (fun str -> str|> BigInteger.Parse) |> fun xs -> (xs[0], xs[1], xs[2]),xs[3])

    0

    let dict = Dictionary<string,string>()
    dict["model"] <- "true"
    //let opt = Gs.context().MkOptimize()
    let opt = Gs.overrideContext(dict).MkOptimize()
    //Solver.create
    
    let ZERO = IntVal 0I
    let ONE = IntVal 1I

    let varX = Int "x"
    let varY = Int "y"
    let varZ = Int "z"

    let ctx = Gs.context()
    
    let intSort = ctx.MkIntSort() :> Sort
    let boolSort = ctx.MkBoolSort() :> Sort
    let realSort = ctx.MkRealSort() :> Sort


    //opt.Add(varX >=. 0I |> asBoolExpr)
    //opt.Add(varX <=. 16I |> asBoolExpr)
    //opt.MkMaximize(varX |> asIntExpr)
    //let status = opt.Check()
    //let resModel = opt.Model

    let zAbs (a: Int) = Z3.CreateFunction<Int>("zabs", intSort, IIF_Int (a >=. ZERO , a , (0I-a)))
    let zDist (a: Int, b: Int) = Z3.CreateFunction<Int>("zdist", intSort, zAbs(a - b))
    let zDist2 (a: Int) (b: Int) = Z3.CreateFunction<Int>("zdist2", intSort, zAbs(a - b))

    //let rule1a = IIF_Int((zDist(IntVal 10I,varX) + zDist(IntVal 12I,varY) + zDist(IntVal 12I,varZ)) <=. IntVal 2I, IntVal 1I, IntVal 0I)
    //let rule1b = IIF_Int((zDist2 (IntVal 10I) varX + zDist2 (IntVal 12I) varY + zDist2 (IntVal 12I) varZ) <=. IntVal 2I, IntVal 1I, IntVal 0I)
    let rule1c = ctx.MkITE (ctx.MkGe((varX |> asIntExpr) , (ctx.MkIntConst "10")) , (ctx.MkIntConst "0"), (ctx.MkIntConst "1"))
    //let rule2 = IIF_Int(zDist(IntVal 12I,varX) + zDist(IntVal 14I,varY) + zDist(IntVal 12I,varZ) <=. IntVal 2I, IntVal 1I, IntVal 0I)

    let combined = rule1c// + rule2
    //opt.Add(combined)
    opt.Add(varX <=. 5I |> asBoolExpr)
    opt.Add(varX >=. 15I |> asBoolExpr)
    //opt.MkMaximize(combined |> asIntExpr)
    opt.MkMaximize(rule1c)
    //opt.MkMaximize(varX |> asIntExpr)
    let status = opt.Check()
    let resModel = opt.Model

    // -8885, -2437, 0, 2
    let x,y,z,c = resModel.Eval(varX |> asIntExpr),resModel.Eval(varY |> asIntExpr),resModel.Eval(varZ |> asIntExpr),resModel.Eval(combined)
    let wut = resModel.Eval((zDist2 (IntVal 10I) varX + zDist2 (IntVal 12I) varY + zDist2 (IntVal 12I) varZ) |> asIntExpr)
    
    let zDrones = [| 
            for (dx,dy,dz),dr in lns do
                let x,y,z,r = IntVal dx, IntVal dy, IntVal dz, IntVal dr
                let condition = (zDist(x,varX) + zDist(y,varY) + zDist(z,varZ) <=. r)
                let zIsInRange = IIF_Int(condition, ONE, ZERO) 
                yield zIsInRange
        |]

    let zDronesInRangeSum = Gs.context().MkAdd(zDrones |> Array.map (asIntExpr >> (fun x -> x :> ArithExpr))) :?> IntExpr |> IntExpr


    //opt.MkMaximize(zDronesInRangeSum)
    let varDronesInRangeSum = Int "dronesInRangeSum"
    let ruleDronesInRangeSum : BoolExpr = varDronesInRangeSum =. zDronesInRangeSum |> asBoolExpr
    opt.Add(ruleDronesInRangeSum)
    opt.MkMaximize(varDronesInRangeSum |> asIntExpr)
    
    //opt.MkMinimize(varX + varY + varZ |> asIntExpr)
    let varDistFromZero = Int "distFromZero"
    opt.Add(varDistFromZero =. (varX + varY + varZ) |> asBoolExpr)
    opt.MkMinimize(varDistFromZero |> asIntExpr)
    
    let status = opt.Check()
    let resModel = opt.Model

    let resDistFromZero = resModel.Eval(varX + varY + varZ |> asIntExpr)

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


