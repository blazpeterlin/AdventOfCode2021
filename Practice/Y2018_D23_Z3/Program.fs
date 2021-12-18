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
    //dict["model"] <- "true"
    //let s = Gs.overrideContext(dict)
    let huh = s = Gs.context()
    let opt = Gs.context().MkOptimize()

    let varX = Int "x"
    let varY = Int "y"
    let varZ = Int "z"

    let ZERO = IntVal 0I
    let ONE = IntVal 1I

    let intSort = Gs.context().MkIntSort() :> Sort
    let boolSort = Gs.context().MkBoolSort() :> Sort
    let realSort = Gs.context().MkRealSort() :> Sort

    let zAbs (a: Int) = Z3.CreateFunction<Int>("zabs", intSort, IIF_Int (a >=. ZERO , a , (0I-a)))
    let zDist (a: Int, b: Int) = Z3.CreateFunction<Int>("zdist", intSort, zAbs(a - b))
    
    let zDrones = [| 
            for (dx,dy,dz),dr in lns do
                let x,y,z,r = IntVal dx, IntVal dy, IntVal dz, IntVal dr
                let condition = (zDist(x,varX) + zDist(y,varY) + zDist(z,varZ) <=. r)
                let zIsInRange = IIF_Int(condition, ONE, ZERO) 
                yield zIsInRange
        |]

    let zDronesInRangeSum = Gs.context().MkAdd(zDrones |> Array.map (asIntExpr >> (fun x -> x :> ArithExpr)))

    //let varDistFromZero = Int "distFromZero"
    //let ruleDistFromZero = varDistFromZero =. (varX + varY + varZ) |> asBoolExpr
    //opt.Add(ruleDistFromZero)

    opt.MkMaximize(zDronesInRangeSum)
    opt.MkMinimize(varX + varY + varZ |> asIntExpr)
    
    let status = opt.Check()
    let resModel = opt.Model

    let resDistFromZero = resModel.Eval(varX + varY + varZ |> asIntExpr)

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


