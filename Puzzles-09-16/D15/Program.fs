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

//type State = { Node:string; }

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"

    let lns = 
        C.readLines inputFile
        |> C.toPosMap (fun str -> str.ToCharArray() |> List.ofArray |> List.map (fun ch -> ch.ToString() |> int))
        //|> List.map (
        //    fun (ln:string) -> 
        //        ln |> 
        //    )
    0



    let pos0 = (0,0)
    let maxX1 = lns.Keys |> List.ofSeq |> List.map fst |> List.max
    let maxY1 = lns.Keys |> List.ofSeq |> List.map snd |> List.max

    let heuristic (x : int,y  : int) =
        let a,b = (-maxX1, -maxY1) +.. (x,y)
        a+b

    let getSteps1 pos =
        C.MOVES_PLUS
        |> List.map (fun move -> move +.. pos)
        |> List.filter (fun newPos -> lns.ContainsKey newPos)
        |> List.map (fun newPos -> (newPos, lns[newPos]))
        |> Seq.ofList

    let resPath = 
        pos0 
        |> AStar.unfold true getSteps1 heuristic id (fun x -> x.Heuristic = 0)
        |> Seq.filter (fun x -> x.IsGoal)
        |> Seq.head

    let res1 = 
        resPath.ChainReverse
        |> Seq.rev
        |> Seq.skip 1
        |> Seq.map (fun cr -> lns[cr.State])
        |> Seq.sum

    let lenX = maxX1+1
    let lenY = maxY1+1

    let map2 = 
        seq {
            for a in [0..4] do
                for b in [0..4] do
                    for x in [0..maxX1] do
                        for y in [0..maxY1] do
                            yield ((a*lenX+x,b*lenY+y), (lns[x,y]-1+a+b)%9 + 1)
        }
        |> Map.ofSeq
        
    
    let maxX2 = map2.Keys |> List.ofSeq |> List.map fst |> List.max
    let maxY2 = map2.Keys |> List.ofSeq |> List.map snd |> List.max
    let posN2 = (maxX2,maxY2)
    

    // ugly duplicated code ahead, since this was all entered for global leaderboard speed.

    let heuristic2 (x : int,y  : int) =
        let a,b = (-maxX2, -maxY2) +.. (x,y)
        a+b

    let getSteps2 pos =
            C.MOVES_PLUS
            |> Seq.map (fun move -> move +.. pos)
            //|> List.filter (fun newPos -> map2.ContainsKey newPos)
            //|> List.map (fun newPos -> (newPos, map2[newPos]))
            |> Seq.choose (fun newPos ->
                match map2.TryFind newPos with
                | Some oldVal -> Some (newPos, oldVal)
                | None -> None
                )
            |> Seq.cache
            //|> Seq.ofList

    let r2 = 
        pos0 
        |> AStar.unfold true getSteps2 heuristic2 id (fun x -> x.Heuristic = 0)
        |> Seq.cache

    //let r2c = r2 |>Seq.length

    let res2Path = 
        r2
        |> Seq.filter (fun x -> x.IsGoal)
        |> Seq.head

    let res2 = 
        res2Path.ChainReverse
        |> Seq.rev
        |> Seq.skip 1
        |> Seq.map (fun cr -> map2[cr.State])
        |> Seq.sum


    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


