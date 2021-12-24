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

type ENV = T | P

// type State = { EL:int; }

[<EntryPoint>]
let main argv =    
    let env =P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let lns = 
        C.readLines inputFile
        |> List.map (fun ln -> ln)

    0

    // figure out the exact polynomial equation for part 2
    
    let getValInt (r:Result) =
        match r with 
        | Func(_) -> failwith "Unexpected func"
        | Const(x) -> x.ToString() |> simplified |> asNumber |> int
    let getValDouble (r:Result) =
        match r with 
        | Func(_) -> failwith "Unexpected func"
        | Const(x) -> x.ToString() |> simplified |> asNumber |> double

    let a = Real("a")
    let b = Real("b")
    let c = Real("c")
    
    let z3res = 
        Z3.Solve(
            c =. 0.0,
            a + b + c =. 1.0,
            4.0 * a + 2.0 * b + c =. 3.0
            )


    let mappedRes =
        match z3res with
        | NoSolution | Unknown -> failwith ("fail " + z3res.ToString())
        | Solution(sfrs) ->
            let m = sfrs |> List.map (fun (s,f,r) -> (s.ToString(), getValDouble r)) |> Map.ofList
            m
    
    let val_a = mappedRes["a"]
    let val_b = mappedRes["b"]
    let val_c = mappedRes["c"]

    let angouriRes = 
        [
            "c"
            "a + b + c - 1"
            "4a + 2b + c - 3"
        ]
        |> List.map parsed
        |> AngouriMath.Core.EquationSystem
        |> fun x -> x.Solve("a", "b", "c")
    
    printfn "%O" (simplified (solutions "x" "x^2 + 2 a x + a^2 = 0"))

    //let o = Gs.context().MkOptimize()
    //Z3.S

    //let dog = Int("dog")
    //let cat = Int("cat")
    //let mouse = Int("mouse")
    
    //let res = 
    //    Z3.Solve(
    //        dog >=. 1I,   // at least one dog
    //        cat >=. 1I,   // at least one cat
    //        mouse >=. 1I, // at least one mouse
    //        // we want to buy 100 animals
    //        dog + cat + mouse =. 100I,
    //        // We have 100 dollars (10000 cents):
    //        // dogs cost 15 dollars (1500 cents),
    //        //   cats cost 1 dollar (100 cents), and
    //        //   mice cost 25 cents
    //        1500I * dog + 100I * cat + 25I * mouse =. 10000I)

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


