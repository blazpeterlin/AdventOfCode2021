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

    // C.toPosMap (fun (pos) -> "")
    
    let lns = 
        C.readLines inputFile
        |> List.map (
            fun (ln:string) -> 
                ln |> C.splitCh "-" |> fun xs -> (xs[0], xs[1])
            )

    0

    let map = 
        lns 
        |> List.append (lns |> List.map (fun (x,y) -> (y,x)))
        |> List.groupBy fst 
        |> List.map (fun (x, ys) -> (x, ys |> List.map snd)) 
        |> Map.ofSeq

    let st0 = ("start", [("start",1)] |> Map.ofList, ["start"])

    let getSteps (pos, (st:Map<string,int>), vstd) =
        if pos = "end" then Seq.empty else
        let r =
            match map.TryFind pos with
            | None -> Seq.empty
            | Some mapped ->
                mapped
                |> Seq.filter (fun key -> 
                    if key<>"start" && key<>"end" && key.ToLower() = key && (st.ContainsKey "smallCave")=false then true else
                    match st.TryFind key with | None -> true | Some x -> key.ToLower()<>key || (x<1 && not(st.ContainsKey "smallCave")))
                |> Seq.map (fun x -> 
                    let hasSmallCave = x <> "start"  && x <> "end" && x.ToLower() = x && st.ContainsKey "smallCave"
                    let addSmallCave = x <> "start"  && x <> "end" && x.ToLower() = x && st.ContainsKey x
                    let nextVal = match st.TryFind x with | None -> (if hasSmallCave then 1 else 0) | Some y  -> (if x.ToLower()=x then 1 else 0)
                    let st1 = if addSmallCave then st |> Map.add "smallCave" 1 else st
                    ((x, st1 |> Map.add x nextVal, x::vstd),1)
                )
        r

    let r1 = 
        st0
        |> 
            AStar.unfold 
                true
                getSteps
                (fun _ -> 0) 
                (fun (gpos, gst, vstd) -> vstd (*gst |> Map.toList |> List.sort |> List.append [("|",-1)] |> List.append (vstd |> List.map (fun v -> (v,-2)))*))
                (fun g -> (match g.State with | gpos, _, _ -> gpos) = "end")
        |> Seq.filter (fun g -> g.IsGoal)
        |> List.ofSeq
        //|> Seq.length

    let res1 = r1 |>Seq.length

    //let t = r1 |> List.map (fun x -> x.ChainReverse |> List.map (fun cr -> cr.State |> fun (x,y,z) ->x)) |> List.distinct |>List.map (List.rev >>List.fold (fun x y -> x+"," + y) "") |> List.sort
    //let t2 = t |> List.fold (fun x y -> x + Environment.NewLine + y) ""

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


