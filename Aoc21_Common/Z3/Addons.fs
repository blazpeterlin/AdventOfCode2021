module Microsoft.Z3.Addons

open System
open Microsoft.Z3
open Microsoft.Z3.Bool
open Microsoft.Z3.Int
open Microsoft.Z3.Real
open Microsoft.Z3.BitVec

type Opt() =
    let o:Optimize = Gs.context().MkOptimize()
    member Opt.Add (b:Bool) = 
        o.Add(b |> asBoolExpr) |> ignore
    member Opt.AddAll (bs:Bool seq) = 
        bs |> Seq.iter (fun b -> Opt.Add b)
    member Opt.Check() = o.Check()
    member Opt.CheckOrFail() = o.Check() |> function | Status.UNSATISFIABLE -> failwith "unsatisfiable" | Status.UNKNOWN -> failwith "unknown" | Status.SATISFIABLE -> () |> ignore | huh -> failwith "huh"
    member Opt.Maximize (th:Theory) = o.MkMaximize(th.Expr) |> ignore
    member Opt.Minimize (th:Theory) = o.MkMinimize(th.Expr) |> ignore
    member Opt.Eval (th:Theory) = o.Model.Eval(th.Expr)
    member Opt.Model = o.Model
    member Opt.Push = o.Push
    member Opt.Pop = o.Pop
    
// Z3 if-then-else  (a ??> b --> c)
let (??>)<'A when 'A :> Theory> (b:Bool)  (expr1:'A,expr2:'A) : 'A = 
    match (expr1 :> obj),(expr2 :> obj) with
    | (:? Bool as b1),(:? Bool as b2) -> BoolUtils.createITE (b |> asBoolExpr) (b1 |> asBoolExpr) (b2 |> asBoolExpr) |> (fun res -> res :> obj :?> 'A)
    | (:? Int as i1),(:? Int as i2) -> IntUtils.createITE (b |> asBoolExpr) (i1 |> asIntExpr) (i2 |> asIntExpr) |> (fun res -> res :> obj :?> 'A)
    | (:? Real as r1),(:? Real as r2) -> RealUtils.createITE (b |> asBoolExpr) (r1 |> asRealExpr) (r2 |> asRealExpr) |> (fun res -> res :> obj :?> 'A)
    | (:? BitVec as bv1),(:? BitVec as bv2) -> BitVecUtils.createITE (b |> asBoolExpr) (bv1 |> asBvExpr) (bv2 |> asBvExpr) |> (fun res -> res :> obj :?> 'A)
    | _ ->failwith "Failed to match types in ?>"
let (-->) (expr1) (expr2) = (expr1,expr2)