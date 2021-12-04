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

// type State = { EL:int; }



[<EntryPoint>]
let main argv =    
    let env =P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable lns = 
        C.readLines inputFile
        |> List.map (fun ln -> ln)
    0

    let nums = lns[0] |> C.splitCh "," |> List.map int

    let boardsTxt = lns |> List.skip 1 |> List.chunkBySize 6

    let boards = 
        boardsTxt
        |> List.map (fun btxt0 ->
            let btxt = btxt0 |>List.skip 1
            let bnums = btxt |> List.map (fun ln -> ln |> C.splitCh " " |> List.map int)
            let posByNum = 
                bnums
                |> List.mapi (fun y row -> 
                    row |> List.mapi (fun x elt ->
                        (elt,(x,y))))
                |> List.concat
                |> Map.ofList
            posByNum)

    let numBoards = boards |> List.length

    let filledPosByBoard : Map<int, (int*int) list> =
        boards
        |> List.mapi (fun i b -> (i, []))
        |> Map.ofList

    let fillBoards (fpbb:Map<int, (int*int) list>) num =
        let r2 = 
            fpbb.Keys
            |> List.ofSeq
            |> List.fold 
                (fun (iterF:Map<int, (int*int) list>) key ->
                    let b = boards[key]
                    if b.ContainsKey num then iterF.Add (key,(List.append iterF[key] [b[num]])) else iterF
                
                ) fpbb
        r2

    let getIdxOfWinningBoard (fpbb:Map<int, (int*int) list>) =
        [0..numBoards-1]
        |> List.map (
            fun key ->
                let allPos = fpbb[key]
                let countByX =  allPos |> List.countBy fst
                let countByY =  allPos |> List.countBy snd
                let hasAllX = countByX |> List.filter( fun (x,num) -> num=5) |> (List.isEmpty >> not)
                let hasAllY = countByY |> List.filter( fun (y,num) -> num=5) |> (List.isEmpty >> not)
                (key, hasAllX || hasAllY)
            )
        |> List.filter( fun (key,isWin) -> isWin)
        |> List.tryHead

    
    
    let getAllIdxOfWinningBoard (fpbb:Map<int, (int*int) list>) =
        [0..numBoards-1]
        |> List.map (
            fun key ->
                let allPos = fpbb[key]
                let countByX =  allPos |> List.countBy fst
                let countByY =  allPos |> List.countBy snd
                let hasAllX = countByX |> List.filter( fun (x,num) -> num=5) |> (List.isEmpty >> not)
                let hasAllY = countByY |> List.filter( fun (y,num) -> num=5) |> (List.isEmpty >> not)
                (key, hasAllX || hasAllY)
            )
        |> List.filter( fun (key,isWin) -> isWin)

    let (winningStep, firstWinningBoards) =
        nums
        |> Seq.scan fillBoards filledPosByBoard
        |> Seq.skip 1
        |> Seq.mapi (fun step r -> (step,r))
        |> Seq.filter (fun (step,fpbb) -> match getIdxOfWinningBoard fpbb with | Some _ -> true | None ->false)
        |> Seq.head

    let callingNum = nums[winningStep]
    let boardIdx = getIdxOfWinningBoard firstWinningBoards |> function | Some x ->x |> fst
    let allBoardNums = boards[boardIdx].Keys |>List.ofSeq
    let unmarkedBoardNums = allBoardNums |> List.except (nums |> List.take (winningStep+1))


    // not 20213
    let res1=  (unmarkedBoardNums |> List.sum) * callingNum

    let allBoardsExceptLast =
        nums
        |> Seq.scan fillBoards filledPosByBoard
        |> Seq.skip 1
        |> Seq.mapi (fun step r -> (step,r))
        |> Seq.filter (fun (step,fpbb) -> getAllIdxOfWinningBoard fpbb |> (fun r -> r.Length >= numBoards-1))
        |> Seq.map (fun (step,fpbb) -> getAllIdxOfWinningBoard fpbb)
        |> Seq.head
        |> List.map fst

    let lastBoardIdx = [0..numBoards-1] |> List.except allBoardsExceptLast |> List.head
    
    let (lastCallingNumIdx, lastBoardSt) =
        nums
        |> Seq.scan fillBoards filledPosByBoard
        |> Seq.skip 1
        |> Seq.mapi (fun step r -> (step,r))
        |> Seq.filter (fun (step,fpbb) -> getAllIdxOfWinningBoard fpbb |> (fun r -> r.Length >= numBoards))
        |> Seq.head

    let lastCallingNum = nums[lastCallingNumIdx]
    let lastBoardState = lastBoardSt |> fun m -> m[lastBoardIdx]
    let lastBoardNums = boards[lastBoardIdx].Keys |>List.ofSeq
    let unmarkedLastBoardNums = lastBoardNums |> List.except (nums |> List.take (lastCallingNumIdx+1))
    
    let res2=  (unmarkedLastBoardNums |> List.sum) * lastCallingNum

    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

