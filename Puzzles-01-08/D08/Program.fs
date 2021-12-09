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
    
    let lns = 
        C.readLines inputFile
        |> List.map (fun ln -> ln |>C.splitCh "|" |> fun (x::y::[]) -> (x|>C.splitCh " ",y|>C.splitCh " "))

    0

    let allChars = "abcdefg".ToCharArray() |> List.ofArray

    let rec generateCombinations (chList : char list) : char list list =
        match chList with
        | [x] -> [[x]]
        | x -> 
            [0..x.Length-1]
            |> List.map (fun idx ->
                let ch = chList[idx]
                let innerChList = chList |> List.except [ch]
                let r = 
                    generateCombinations innerChList
                    |> List.map (fun arr -> ch::arr)
                r
            )
            |> List.concat

    let allCombinations = generateCombinations allChars

    let correctPairing = 
        [("abcefg",0);("cf",1);("acdeg",2);("acdfg",3);("bcdf",4);("abdfg",5);("abdefg",6);("acf",7);("abcdefg",8);("abcdfg",9)]
        |> List.sortBy fst
    let cpKeys = correctPairing |> List.map fst
    let correctMap = correctPairing |> Map.ofList

    let allMappings = 
        List.allPairs [allChars] allCombinations
        |> List.map (fun (chFrom, chTo) ->
            List.zip chFrom chTo
            |> Map.ofList
            )
    
    let mapStrings (strs: string list) (mp : Map<char,char>) = strs |> List.map (fun str -> str.ToCharArray() |> Array.map (fun ch -> mp[ch]) |> String)
    let sortStrs (strs : string list) = strs |> List.map (fun str -> str.ToCharArray() |> Array.sort |> String) |> List.sort

    let isValidMapping (def: string list) (mp : Map<char,char>) =
        let mappedDef = mapStrings def mp |> sortStrs
        if mappedDef[0] = "abcdefg"
        then 0
        else 0

        //if ([0..8] |> List.forall (fun idx -> mappedDef[idx]=cpKeys[idx]))
        //then 0
        //else 0

        if mappedDef = cpKeys then 
            0
            true
        else 
        false

    let parsed =
        lns
        |> List.map (fun (defs,statement) ->
            let mp = 
                allMappings
                |> List.filter (fun mpp -> isValidMapping defs mpp)
                |> List.exactlyOne

            let mappedSt = mapStrings statement mp
            let mapNums = mappedSt |> List.map (fun str -> str.ToCharArray() |> Array.sort |> String) |>List.map (fun str ->correctMap[str])
            (defs,statement,mp,mapNums)
            )
        
    let res1 =
        parsed
        |> List.map (fun (_,_,_,nums) -> nums |> List.filter (fun num -> num=1 || num=4 ||num=7 ||num=8) |> List.length)
        |> List.sum

    
    let res2 =
        parsed
        |> List.map (fun (_,_,_,nums) -> nums[0]*1000 + nums[1]*100 + nums[2]*10 + nums[3])
        |> List.sum


    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


