
namespace Aoc21_Common

open System.Collections.Generic


type DijkstraAlg<'T> = 
    { pq: (PathPart<'T> list) BinomialHeapPQ.outerheap; heuristic: 'T->int; } 

type DijkstraVisited<'T,'U> when 'U:comparison =
    { mutable visited: Set<'U>; state2visited: 'T->'U; }

[<StructuredFormatDisplay("COST={TotalCost} H={Heuristic} | {State}")>]
type DijkstraResult<'T> =
    { IsGoal: bool; State:'T; TotalCost: int; Heuristic: int; ChainReverse: PathPart<'T> list }


[<RequireQualifiedAccess>]
module Dijkstra =
    let init (state0:'a) (heuristic:'a->int) (state2visited:'a->'b) =
        let pq = [[{State=state0; CostSoFar=0; CostAhead=heuristic state0; }]] |> PQ.ofSeq
        let visited0 = Set.empty //[state2visited state0] |> set
        (
            { pq=pq; heuristic=(heuristic); },
            { state2visited=state2visited; visited=visited0 }
        )

    let iterate (dvst: DijkstraVisited<'T,'U>) (step:'T -> ('T*int) seq) (dalg: DijkstraAlg<'T>) =
        let (topEltChainOpt, nextPq) = dalg.pq |> PQ.dequeue
        if topEltChainOpt=None then None else
        let topEltChain : PathPart<'T> list = topEltChainOpt.Value.v
        let topElt = topEltChain |> List.head
        let { State=topEltState; CostSoFar=costSoFar; CostAhead=h } = topElt
        if dvst.visited.Contains (topEltState |> dvst.state2visited) then Some (None, { dalg with pq=nextPq }) else
        dvst.visited <- dvst.visited.Add (topEltState |> dvst.state2visited)

        let addedElts = topEltState |> step
        
        let resDijkstra =
            addedElts 
            |> Seq.fold (fun dai (actualElt,moveCost) -> 
                    let costAhead = actualElt |> dai.heuristic 
                    { dai with
                        pq=PQ.insert dai.pq ({ State=actualElt; CostSoFar=costSoFar+moveCost; CostAhead=costAhead }::topEltChain)
                    }
                )
                dalg
        
        Some (Some (topEltState, (costSoFar, h), topEltChain), resDijkstra)

    let unfold (step:'T -> ('T*int) seq) state0 heuristic state2visited (isGoal:{| State:'T; TotalCost:int; Heuristic:int |}->bool)  =
        let (dalg, dvst) = init state0 heuristic state2visited
        dalg
        |> Seq.unfold (iterate dvst step)
        |> Seq.choose id
        |> Seq.map (fun (state, (totalCost,heuristic), chain) -> 
                let g = isGoal {| State=state; TotalCost=totalCost; Heuristic=heuristic; |}
                //(state,(totalCost,heuristic),g)
                { State=state; TotalCost=totalCost; Heuristic=heuristic; IsGoal=g; ChainReverse=chain }
            )

