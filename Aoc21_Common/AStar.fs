
namespace Aoc21_Common

open System.Collections.Generic


type AStarAlg<'T> = 
    { pq: (PathPart<'T> list) BinomialHeapPQ.outerheap; heuristic: 'T->int; } 

type AStarVisited<'T,'U> when 'U:comparison =
    { mutable visited: Set<'U>; state2visited: 'T->'U; }

[<StructuredFormatDisplay("COST={TotalCost} H={Heuristic} | {State}")>]
type AStarResult<'T> =
    { IsGoal: bool; State:'T; TotalCost: int; Heuristic: int; ChainReverse: PathPart<'T> list }

type AStarGoalParams<'T> = { State:'T; TotalCost:int; Heuristic:int }


[<RequireQualifiedAccess>]
module AStar =
    let init (state0:'a) (heuristic:'a->int) (state2visited:'a->'b) =
        let pq = [[{State=state0; CostSoFar=0; CostAhead=heuristic state0; }]] |> PQ.ofSeq
        let visited0 = Set.empty //[state2visited state0] |> set
        (
            { pq=pq; heuristic=(heuristic); },
            { state2visited=state2visited; visited=visited0 }
        )

    let iterate (keepChain: bool) (dvst: AStarVisited<'T,'U>) (step:'T -> ('T*int) seq) (dalg: AStarAlg<'T>) =
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
                    let nextTopEltChain = if keepChain then topEltChain else []
                    { dai with
                        pq=PQ.insert dai.pq ({ State=actualElt; CostSoFar=costSoFar+moveCost; CostAhead=costAhead }::nextTopEltChain)
                    }
                )
                dalg
        
        Some (Some (topEltState, (costSoFar, h), topEltChain), resDijkstra)

    let unfold (keepChain: bool) (step:'T -> ('T*int) seq) heuristic pathUniqueBy (isGoal:AStarGoalParams<'T>->bool) state0 =
        let (dalg, dvst) = init state0 heuristic pathUniqueBy
        dalg
        |> Seq.unfold (iterate keepChain dvst step)
        |> Seq.choose id
        |> Seq.map (fun (state, (totalCost,heuristic), chain) -> 
                let g = isGoal { State=state; TotalCost=totalCost; Heuristic=heuristic; }
                //(state,(totalCost,heuristic),g)
                { State=state; TotalCost=totalCost; Heuristic=heuristic; IsGoal=g; ChainReverse=chain }
            )

