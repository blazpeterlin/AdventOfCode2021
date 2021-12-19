module Microsoft.Z3.Array

open System
open Microsoft.Z3
open Microsoft.Z3.Bool
open Microsoft.Z3.Int
open Microsoft.Z3.Real





//let mutable counter = -1
type Array1D(expr : Theory array) =

    member val Expr = expr
    override x.ToString() = sprintf "%O" expr 
    (*(|++|)*) 
    static member (=.) (a1: Array1D, a2: Array1D) =
        let ctx = Gs.context()
        a1.Expr |> Array.zip a2.Expr
        |> Array.map (fun (elt1,elt2) -> ctx.MkEq(elt1.Expr, elt2.Expr))
        |> Array.map BoolExpr
        |> Array.map (fun x -> x :> Theory)
        |> Array1D
    static member Z3SUM (a1: Array1D) =
        Gs.context().MkAdd(a1.Expr |> Array.map (fun x -> x.Expr :?> ArithExpr))



let ArrayVal1D (theories: Theory array) = 
  Array1D (theories)
  
let ArrayVal1D_Bool (theories: Bool array) =
    let castedTheories = theories |> Array.map (fun x -> x :> Theory)
    Array1D castedTheories
    
let ArrayVal1D_Int (theories: Int array) =
      let castedTheories = theories |> Array.map (fun x -> x :> Theory)
      Array1D castedTheories
      
let ArrayVal1D_Real (theories: Real array) =
        let castedTheories = theories |> Array.map (fun x -> x :> Theory)
        Array1D castedTheories
  


type Array2D<'I, 'O>(expr: ArrayExpr) = 
  inherit Theory()
  override x.Expr = expr :> Expr
  override x.ToString() = sprintf "%O" expr
  static member FromExpr (e: Expr) = Array2D<_, _>(e :?> ArrayExpr)
  static member (=.)(a1: Array2D<'I, 'O>, a2: Array2D<'I, 'O>) =
      Gs.context().MkEq(a1.Expr, a2.Expr) |> BoolExpr
  static member (<>.)(a1: Array2D<'I, 'O>, a2: Array2D<'I, 'O>) =
      Gs.context().MkDistinct(a1.Expr, a2.Expr) |> BoolExpr

let Array2DExpr<'I, 'O> expr = Array2D<'I, 'O>(expr)
let (|Array2DExpr|) (a: Array2D<_, _>) = a.Expr :?> ArrayExpr

let inline Array<'I, 'O when 'I :> Theory and 'O :> Theory and 'O: (static member FromExpr : Expr -> 'O)>
        (s: string, domain: Sort, range: Sort) = 
    let context = Gs.context()
    context.MkArrayConst(s, domain, range) |> Array2DExpr<'I, 'O>

let inline Select<'I, ^O  when 'I :> Theory and ^O: (static member FromExpr : Expr -> ^O)> 
        (a: Array2D<'I, ^O>) (i: 'I) =
    let expr = Gs.context().MkSelect(a.Expr :?> ArrayExpr, i.Expr)
    (^O : (static member FromExpr : Expr -> ^O) expr)

let inline Store<'I, 'O  when 'I :> Theory and 'O :> Theory> 
        (a: Array2D<'I, 'O>) (i: 'I) (x: 'O) =
    Gs.context().MkStore(a.Expr :?> ArrayExpr, i.Expr, x.Expr) |> Array2DExpr<'I, 'O>
