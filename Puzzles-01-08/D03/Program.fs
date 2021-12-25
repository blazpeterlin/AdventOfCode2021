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

// type State = { EL:int; }



[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable lns = 
        C.readLines inputFile
        //|> Seq.head
        |> List.map (fun x -> x.ToCharArray() |> Seq.map (fun ch -> int (ch.ToString())) |>List.ofSeq)
        //|> C.parseList id
        //|> C.parseMatrix id
    0

    let gamma = 
        lns
        |> List.transpose
        |> List.map (fun nums -> nums |> List.countBy id |> List.sortByDescending snd |> List.head |>fst)
        |> List.map (fun i -> i.ToString())
        |> List.reduce (+)
        |> fun x ->Convert.ToInt32(x, 2)

    let epsilon = 
        lns
        |> List.transpose
        |> List.map (fun nums -> nums |> List.countBy id |> List.sortBy snd |> List.head |>fst)
        |> List.map (fun i -> i.ToString())
        |> List.reduce (+)
        |> fun x ->Convert.ToInt32(x, 2)

    let res1 = gamma * epsilon

    let td = lns |> List.transpose
    
            
    let reduceNums sortType nums  =
        nums
                               |> List.groupBy snd
                               |> List.map snd
                               |> List.map (fun grp -> grp |> List.map (fun g -> (g, Seq.length(grp))))
                               |> List.concat
                               |> List.sortByDescending (snd >> sortType)
                               |> fun y ->     
                                   let bestCount = y |> List.map snd |> List.sortByDescending sortType |> List.head
                                   y 
                                   |> List.filter (fun (num,count) -> count=bestCount)
                                   |> fun z ->
                                       let topNum = z |> List.sortByDescending (fst >> snd >> sortType) |> List.head |> (fst >> snd)
                                       z |> List.filter (fun ((idx1,num1),c1) -> num1=topNum)
                                   //|> List.head
                                   |> List.map fst

    
    let getOneNum sortType ((remainingIdxsNums: int list),totalNumsToTake) (idxNums: (int*int) list)  =
        if (remainingIdxsNums |> List.length)=1 then (remainingIdxsNums,totalNumsToTake) else
        let idx2 = 
            idxNums
            |> List.filter (fun (idx,num) -> remainingIdxsNums |> List.contains idx)
            |> (reduceNums sortType)
            |> List.map fst
        let ri2 =
            remainingIdxsNums
            |> List.filter (fun y -> idx2 |> List.contains y)


        (ri2, totalNumsToTake+1)

    let gamma = 
        td
        |> List.map (List.mapi (fun idx num -> (idx,num)))
        |> List.fold (getOneNum (fun x -> x)) ([0 .. lns.Length],0)
        |> fun (lastIdx::[],lenToTake) ->
            lns[lastIdx] //|> Seq.take lenToTake |> 
            |> List.map (fun i -> i.ToString())
            |> List.reduce (+)
            |> fun x ->Convert.ToInt32(x, 2)
    
    let scrubber = 
        td
        |> List.map (List.mapi (fun idx num -> (idx,num)))
        |> List.fold (getOneNum (fun x -> -x)) ([0 .. lns.Length],0)
        |> fun (lastIdx::[],lenToTake) ->
            lns[lastIdx] //|> Seq.take lenToTake |> 
            |> List.map (fun i -> i.ToString())
            |> List.reduce (+)
            |> fun x ->Convert.ToInt32(x, 2)

    let res2 = gamma * scrubber
    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

