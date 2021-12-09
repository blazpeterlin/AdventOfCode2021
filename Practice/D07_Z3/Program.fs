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
        |> List.map (fun ln -> ln)

    0

    // figure out the exact polynomial equation for part 2
    
    let getVal (r:Result) =
        match r with 
        | Func(_) -> failwith "Unexpected func"
        | Const(x) -> x

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
        | NoSolution -> failwith "nosolution"
        | Unknown -> failwith "unknown"
        | Solution(sfrs) ->
            let m = sfrs |> List.map (fun (s,f,r) -> (s.ToString(), getVal r)) |> Map.ofList
            m

    let va = (mappedRes["a"]) //.ToString() |> float
    let vb = mappedRes["b"].ToString() |> float
    let vc = mappedRes["c"].ToString() |> float

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


