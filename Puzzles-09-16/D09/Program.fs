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
    
    let lns = 
        C.readLines inputFile
        |> (fun xs ->
            let nines = xs[0].ToCharArray() |>Array.map (fun x -> '9') |> String
            [nines]@xs@[nines])
        |> List.mapi (fun y ln -> ("9"+ln+"9").ToCharArray() |> List.ofArray |> List.mapi (fun x ch -> ((x,y),int (ch.ToString()))))
        |> List.concat


    0

    let goodY = [1..(lns |> List.map (fst>>snd) |> List.max)-1]
    let goodX = [1..(lns |> List.map (fst>>fst) |> List.max)-1]

    let map = lns |> Map.ofList

    let allNeighbours = [(-1,0);(1,0);(0,-1);(0,1)]

    let goodPos = 
        List.allPairs goodX goodY
        |> List.filter (fun (x,y) -> 
            let isGood =
                allNeighbours
                |> List.forall (fun n -> 
                    let nPos = (x,y)+..n
                    map[(x,y)]< map[nPos]
                    )
                
            isGood
            )

    let res1 =
        goodPos
        |> List.map (fun (x,y) -> map[(x,y)]+1)
        |> List.sum

    let tryNextBasinElt (edges : (int*int) list, visited: (int*int) Set) =
        if List.isEmpty edges then None else

        let (x,y) = List.head edges
        let tail = List.tail edges

        if map[(x,y)]=9 then Some (visited, (tail, visited)) else

        let nextCandidates = 
            allNeighbours
            |> List.map ((+..)(x,y))
            |> List.filter (visited.Contains >> not)

        let visited2 = visited.Add (x,y)
        Some (visited2, (List.append nextCandidates tail, visited2))


    let getBasin (x,y) =
        let basinSet = [(x,y)]
        let visited = [(x,y)] |> set

        (basinSet, visited)
        |> List.unfold tryNextBasinElt
        |> List.last
        |> List.ofSeq
        
    let allBasins = 
        goodPos
        |> List.map getBasin
        |> List.sortByDescending (fun basinPos ->basinPos.Length)

    let top3b = allBasins |> List.take 3

    let t1::t2::t3::[] = top3b

    let res2 = t1.Length * t2.Length * t3.Length

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


