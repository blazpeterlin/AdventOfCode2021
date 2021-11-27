
namespace Aoc21_Common

open System.Collections.Generic

//type GraphNode<'ELT> = 'ELT * int * int * ('ELT seq)

type PathPart<'T> = { State:'T; CostSoFar:int; CostAhead:int; }

[<RequireQualifiedAccess>]
module PQ =
    let empty = BinomialHeapPQ.empty
    let isEmpty = BinomialHeapPQ.isEmpty

    let merge = BinomialHeapPQ.merge

    let insert heap (node:PathPart<'ELT> list) =
        let { State=elt; CostSoFar=costSoFar; CostAhead=costAhead } = (node |> List.head)
        heap |> BinomialHeapPQ.insert (costSoFar+costAhead) node

    let insertMany heap (sq: PathPart<'ELT> list seq) =
        sq
        |> Seq.fold (fun hp ({ State=elt; CostSoFar=costSoFar; CostAhead=costAhead}::lst) -> BinomialHeapPQ.insert (costSoFar+costAhead) ({ State=elt; CostSoFar=costSoFar; CostAhead=costAhead}::lst) hp) heap

    let ofSeq (sq: PathPart<'ELT> list seq) =
        sq 
        |> Seq.filter (function | [] -> false | _ -> true)
        |> insertMany BinomialHeapPQ.empty

    let dequeue heap =
        let elt = BinomialHeapPQ.getMin heap
        (elt, BinomialHeapPQ.deleteMin heap)