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
                ln |> C.splitCh "-" |> fun xs -> (xs[0], xs[1])
            )

    0

    let map = 
        lns 
        |> List.append (lns |> List.map (fun (x,y) -> (y,x)))
        |> List.groupBy fst 
        |> List.map (fun (x, ys) -> (x, ys |> List.map snd)) 
        |> Map.ofSeq


    let getNextStates (st:State) =
        if st.Node = "end" then Seq.empty else

        let neighbours = map.TryFind st.Node
        if neighbours = None then Seq.empty else

        neighbours.Value
        |> Seq.filter (fun key ->
            if key="start" then false else

            let isSmall = key <> "end" && key.ToLower() = key

            if isSmall && not(st.AlreadyDoubledSmallCave) 
            then true 
            else not(st.ForbiddenNodes.Contains key)
            )
        |> Seq.map (fun key -> 
            let isSmall = key <> "end" && key.ToLower() = key
            let nextDoubledCave = isSmall && st.ForbiddenNodes.Contains key

            let nextForbidden = if isSmall then st.ForbiddenNodes.Add key else st.ForbiddenNodes
            let nextSt = { Node=key ; ForbiddenNodes = nextForbidden ; History= key::st.History ; AlreadyDoubledSmallCave = st.AlreadyDoubledSmallCave || nextDoubledCave }
            (nextSt, 0)
        )
        
    let heuristic = (fun st -> 0)
    let pathUniqueBy (st:State) = st.History
    let isGoal (g:AStarGoalParams<State>) = g.State.Node = "end"

    let calcRes =
        AStar.unfold false getNextStates heuristic pathUniqueBy isGoal
        >> Seq.filter (fun g -> g.IsGoal)
        >> Seq.length
        
    let st0_part1 =  { Node="start" ; ForbiddenNodes=Set.empty ; History=[] ; AlreadyDoubledSmallCave= true }
    let res1 = calcRes st0_part1
        
    let st0_part2 =  { st0_part1 with AlreadyDoubledSmallCave = false; }
    let res2 = calcRes st0_part2
    

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


