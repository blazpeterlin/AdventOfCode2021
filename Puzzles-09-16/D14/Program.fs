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

//type State = { Node:string; }

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"

    // C.toPosMap (fun (str) -> str.ToCharArray())
    
    let lns = 
        C.readLines inputFile
        |> List.map (
            fun (ln:string) -> 
                ln
            )
    0

    let s0 = lns[0]
    let rules = lns |> List.skip 2 |> List.map (fun ln -> ln |> C.splitCh " ->" |> fun xs -> (xs[0], xs[1])) |> Map.ofList

    //// code used for solving part 1 at 6am
    //let step (str:string) =
    //    let strs = str.ToCharArray() |>List.ofSeq
    //    strs
    //    |> List.windowed 2
    //    |> List.map (fun chs ->  chs |> Array.ofList |> String |> fun s -> match rules.TryFind s with | None -> [s[0]] | Some md -> [s[0];md[0]])
    //    |> List.concat
    //    |> fun prep -> List.append prep [(strs |> List.last)]
    //    |> Array.ofList
    //    |> String

    //let res1str = [1..10] |> List.fold (fun acc _ -> step acc) s0
    //let res1counts = res1str.ToCharArray() |> List.ofArray |> List.countBy id
    //let resMostCommon = res1counts |> List.sortByDescending snd |> List.head
    //let resLeastCommon = res1counts |> List.sortBy snd |> List.head

    //let res1 = (snd resMostCommon) - (snd resLeastCommon)

    let combineGroups grps =
        grps
        |> List.sort
        |> List.fold 
            (fun acc (nextX,nextC) ->
                match acc  with
                | [] -> [(nextX,nextC)]
                | (x,c1)::xs when x=nextX -> (x,c1+nextC)::xs
                | _ -> (nextX,nextC)::acc
            ) 
            []

    let initialStateSmart = lns[0].ToCharArray() |> List.ofSeq |> List.windowed 2 |> List.map (fun chs -> chs |> Array.ofList |> String) |> List.countBy id |> List.map (fun (s,c) -> (s, int64 c))
    let stepSmart (strSmart : (string*int64) list) =
        strSmart
        |> List.map (fun (str, count) -> [str[0..0]+rules[str],count ; rules[str]+str[1..1], count])
        |> List.concat
        |> combineGroups

    let alwaysLastChar = s0.ToCharArray() |> Array.last |> fun ch -> (ch,1L)

    let getSmartResultFromSteps numSteps =
        let groupedCharCounts = 
            seq { 1..numSteps } 
            |> Seq.fold (fun acc _ -> stepSmart acc) initialStateSmart
            |> List.map (fun (chs,count) -> [chs[0],count]) |>List.concat
            |> List.append [alwaysLastChar]
            |> combineGroups

        let (charMostCommon, charCountMostCommon) = groupedCharCounts |> List.sortByDescending snd |> List.head
        let (charLeastCommon, charCountLeastCommon) = groupedCharCounts |> List.sortBy snd |> List.head
        charCountMostCommon - charCountLeastCommon

    let res1smarter = getSmartResultFromSteps 10
    let res2 = getSmartResultFromSteps 40

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


