module Microsoft.Z3.Array

open System
open Microsoft.Z3
open Microsoft.Z3.Bool
open Microsoft.Z3.Int
open Microsoft.Z3.Real
open Microsoft.Z3.BitVec





//let mutable counter = -1
type Array1D<'T when 'T :> Theory>(expr : 'T array) =

    member val Expr = expr
    override x.ToString() = sprintf "%O" expr 
    static member (=.) (a1: Array1D<'T>, a2: Array1D<'T>) =
        let ctx = Gs.context()
        a1.Expr |> Array.zip a2.Expr
        |> Array.map (fun (elt1,elt2) -> ctx.MkEq(elt1.Expr, elt2.Expr))
        |> Array.map BoolExpr
        |> Array.map (fun x -> x :> Theory)
        |> Array1D
    static member DISTINCT (a1: Array1D<'T>) =
        Gs.context().MkDistinct(a1.Expr |> Array.map (fun x -> x.Expr)) |> BoolExpr
    static member private SUM_Int (a1: Array1D<'T>) =
        Gs.context().MkAdd(a1.Expr |> Array.map (fun x -> x.Expr :?> ArithExpr)) :?> IntExpr |> IntExpr
    static member private SUM_Real (a1: Array1D<'T>) =
        Gs.context().MkAdd(a1.Expr |> Array.map (fun x -> x.Expr :?> ArithExpr)) :?> RealExpr |> RealExpr   
    static member private SUM_BitVec (a1: Array1D<'T>) =
        let ctx = Gs.context()
        let allBVs = a1.Expr |> Array.map (fun x -> x.Expr :?> BitVecExpr)
        allBVs |> Array.reduce (fun x y -> ctx.MkBVAdd(x,y)) |> BitVecExpr
    static member SUM (a1: Array1D<'T>) =
        match box a1 with
        | :? Array1D<Int> as arrInt -> Array1D.SUM_Int(arrInt) :> obj :?> 'T
        | :? Array1D<Real> as arrReal -> Array1D.SUM_Real(arrReal) :> obj :?> 'T
        | :? Array1D<BitVec> as arrBV -> Array1D.SUM_BitVec(arrBV) :> obj :?> 'T
        | _ -> failwith "SUM on this type not supported"
    static member OR (a1: Array1D<'T>) =
        Gs.context().MkOr(a1.Expr |> Array.map (fun x -> x.Expr :?> BoolExpr)) |> BoolExpr
    static member AND (a1: Array1D<'T>) =
        Gs.context().MkAnd(a1.Expr |> Array.map (fun x -> x.Expr :?> BoolExpr)) |> BoolExpr
    static member XOR (a1: Array1D<'T>) =
        Gs.context().MkXor(a1.Expr |> Array.map (fun x -> x.Expr :?> BoolExpr)) |> BoolExpr



let ArrayVal1D (theories: 'T array) = 
  Array1D (theories)


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

let inline ArrayZ3<'I, 'O when 'I :> Theory and 'O :> Theory and 'O: (static member FromExpr : Expr -> 'O)>
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
