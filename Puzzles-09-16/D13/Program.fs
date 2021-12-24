open System
open Aoc21_Common
open Aoc21_Common.Operators
open FParsec
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Security.Cryptography
open System.Text

type ENV = T | P

type State = { Node:string; ForbiddenNodes: string Set; History: string list; AlreadyDoubledSmallCave: bool }

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"

    // C.toPosMap (fun (pos) -> "")
    
    let lns = 
        C.readLines inputFile
        |> List.map (
            fun (ln:string) -> 
                ln
            )

    0

    let coords0 = 
        lns 
        |> List.takeWhile(fun ln -> ln<>"") |> List.map (fun x -> x |> C.splitCh "," |> List.map int |> fun xs -> (xs[0], xs[1]))
        |> Set.ofList

    let folds0 = lns |> List.skip (lns |> List.findIndex (fun ln -> ln="") |> (+)1) |> List.map (fun x -> x |> C.splitCh " =" |> fun xs -> (xs[2], int xs[3]))

    let step coords f =
        let (f0coord, f0v) = f
        let c1a, c1b =
            let a = coords |> Set.toList |> List.filter (fun ((x,y)) -> if f0coord="y" then y < f0v else x < f0v)
            let b = 
                coords 
                |> Set.toList
                |> List.filter (fun ((x,y)) -> if f0coord="y" then y > f0v else x > f0v)
                |> List.map (fun ((x,y)) -> 
                    if f0coord="y" 
                    then ((x,f0v-(y-f0v)))
                    else ((f0v-(x-f0v),y))
                )
                
            (a,b)
        let nextCoords = c1a |>List.append c1b |> Set.ofList
        nextCoords


    // not 743
    // not 820

    let res1 = step coords0 folds0[0] |> Set.count

    let r2 = folds0 |> List.fold step coords0 |> Set.toList |> List.map (fun (x,y) -> (y,x))
    
    let minx = r2 |>List.map fst |> List.min
    let maxx = r2 |>List.map fst |> List.max
    let miny = r2 |>List.map snd |> List.min
    let maxy = r2 |>List.map snd |> List.max

    let emptymap = [miny..maxy] |> List.allPairs [minx..maxx] |> List.map (fun pos -> (pos,false)) |> Map.ofList

    let r2a = r2 |>List.map (fun pos -> (pos, true))

    let r2b = r2a |> List.fold (fun acc (k,v) -> acc |> Map.add k v) emptymap

    C.print2dMap r2b (fun v -> if v then "█" else " " )

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


