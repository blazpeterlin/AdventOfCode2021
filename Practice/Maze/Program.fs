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
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable lns = 
        C.readLines inputFile
        |> Seq.map (fun str -> str.ToCharArray())
        //|> Seq.head
        //|> List.map (fun x -> x |> C.splitCh "[]")
        //|> C.parseList id
        //|> C.parseMatrix id
        |> Seq.toArray
    0

    let wallMap = 
        lns 
        |> Seq.mapi (fun y row -> row |> Seq.mapi(fun x elt -> ((x,y),elt))) 
        |> Seq.concat 
        |> Seq.filter(fun (pos,v) -> v='1')
        |> Map.ofSeq

    let p0 = (lns[0].Length-1, lns.Length-1)
    let pN = (0,0)

    let getSteps (x,y) =
        let res = 
            [x-1,y;x+1,y;x,y-1;x,y+1]
            |> Seq.filter( fun (x,y) -> x >= 0 && y >= 0 && x < lns[0].Length && y < lns.Length )
            |> Seq.filter (wallMap.ContainsKey >> not)
            |> Seq.map (fun pos -> (pos,1))
        res

    let heuristic (x,y)  =
        let (x0,y0) = pN
        Math.Abs(x-x0) + Math.Abs(y-y0)

    let shortest = 
        p0
        |> AStar.unfold true getSteps heuristic id (fun g -> g.State=pN) 
        |> Seq.filter (fun candidate -> candidate.IsGoal)
        |> Seq.head
        
    let pathS =
        shortest.ChainReverse
        |> Seq.map (fun { State=(x,y); } -> (y,x))
        |> set

    let totalMap =
       lns 
       |> Seq.mapi (fun y row -> row |> Seq.mapi(fun x elt -> ((y,x),elt))) 
       |> Seq.concat 
       |> Seq.filter (fst >> pathS.Contains >> not)
       |> Seq.append (pathS |> Seq.map (fun pos -> (pos,'+')))
       |> Map.ofSeq

    C.print2dMap totalMap


    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
