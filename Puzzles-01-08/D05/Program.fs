open System
open Aoc21_Common
open FParsec
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Security.Cryptography
open System.Text

type ENV = T | P
let internal (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
let internal (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)
let internal ( *..) (x0,y0) (x1,y1) = (x0*x1,y0*y1)

// type State = { EL:int; }

[<EntryPoint>]
let main argv =    
    let env =T
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable lns = 
        C.readLines inputFile
        |> List.map (fun ln -> ln |> C.splitCh " ->," |> List.map int |> fun xs -> ((xs[0],xs[1]),(xs[2],xs[3])))

    0

    let lnsX = lns |> List.filter (fun ((x1,y1),(x2,y2)) -> x1=x2)
    let lnsY = lns |> List.filter (fun ((x1,y1),(x2,y2)) -> y1=y2 && x1 <> x2)

    // [ for x in (min x1 x2)..(max x1 x2) -> (x,y1) ]
    let posX = lnsX |> List.map (fun ((x1,y1),(x2,y2)) -> [(min y1 y2)..(max y1 y2)] |> List.map(fun y -> (x1,y))) |> List.concat
    let posY = lnsY |> List.map (fun ((x1,y1),(x2,y2)) -> [(min x1 x2)..(max x1 x2)] |> List.map(fun x -> (x,y1))) |> List.concat


    let res1 = posX@posY |> List.countBy id |> List.filter( fun (_,cnt) -> cnt>=2) |> List.length

    let lnsD = lns |> List.filter (fun ((x1,y1),(x2,y2)) -> (x2-x1=y2-y1 || x2-x1=y1-y2) && y1<>y2)
    let posD1 = lnsD |> List.map (
            fun ((x1,y1),(x2,y2)) -> 
                let n = (max x2 x1) - (min x2 x1)
                let d = ((x2-x1)/n,(y2-y1)/n)
                let res = [0..n] |> List.map (fun k -> ((k,k) *.. d) +.. (x1,y1))
                res
            )
    let posD = posD1 |> List.concat

    let res2 = posX@posY@posD |> List.countBy id |> List.filter( fun (_,cnt) -> cnt>=2) |> List.length

    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

