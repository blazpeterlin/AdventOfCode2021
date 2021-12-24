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
        |> List.map (
            fun (ln:string) -> 
                ln |> C.splitCh "target area: xy=.," |> List.map int |> (fun xs -> (xs[0], xs[1], xs[2], xs[3]))
            )
    0

    let pos0 = (0,0)

    //// entered at morning for global leaderboard speed. Now replaced by actual parsing code
    //let minx = if env=T then 20 else 207
    //let maxx = if env=T then 30 else 263
    //let miny = if env=T then -10 else -115
    //let maxy = if env=T then -5 else -63
    let (minx,maxx,miny,maxy) = (lns |> List.head)
    
    
    let posCheck (x,y) = 
        x>=minx && x <= maxx && y >= miny && y <= maxy

    let candidates_vx = [minx/abs(minx)..maxx]
    let candidates_vy = [miny .. (maxy-miny+100)]

    let highestY (vx,vy) =
        let (x0,y0) = pos0

        let doStep ((x,y),(vx,vy)) =  
            if posCheck (x,y) then None else

            if x < min minx x0 then None else
            if x > max maxx x0 then None else
            if y < min miny y0 then None else
            //if y > max maxy y0 then None else

            let nextPos = (x+vx,y+vy)
            let nextVX = if vx=0 then 0 else vx - (vx/abs(vx))
            let nextVY = vy-1
            let nextV = (nextVX, nextVY)
            let r = (nextPos,nextV)
            Some (r, r)

        let pns = 
            (pos0, (vx,vy))
            |> Seq.unfold doStep
            |> Seq.cache

        if posCheck (pns |>Seq.last |> fst) then
            Some (pns |> Seq.maxBy (fst >> snd))
        else
            None

    let r1 = 
        Seq.allPairs candidates_vx candidates_vy
        //[(6,9)]
        |> Seq.map (fun (vx,vy) -> highestY (vx,vy))
        |> Seq.choose id
        |> List.ofSeq

    let res1 = 
        r1 |> List.maxBy (fst >> snd) |> fst |>snd

    let res2 = r1 |> List.length

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


