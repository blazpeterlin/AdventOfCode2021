
namespace Aoc21_Common

open System.Collections.Generic
open System

[<CustomComparison; StructuralEquality>]
type PathPart<'T> = { State:'T; CostSoFar:int; CostAhead:int; }
                        interface IComparable<PathPart<'T>> with
                            member this.CompareTo other =
                                compare  (other.CostSoFar + other.CostAhead) (this.CostSoFar + this.CostAhead)
                        interface IComparable with
                            member this.CompareTo(obj: obj) =
                                match obj with
                                | :? PathPart<'T> -> 
                                    let other = unbox<PathPart<'T>> obj
                                    compare  (other.CostSoFar + other.CostAhead) (this.CostSoFar + this.CostAhead)
                                | _ -> invalidArg "obj" "Must be of type PathPart"


[<RequireQualifiedAccess>]
module PQ =

    ////let empty = BinomialHeapPQ.empty
    ////let isEmpty = BinomialHeapPQ.isEmpty
    ////let merge = BinomialHeapPQ.merge
    
    //let insert heap (node:PathPart<'ELT> list) =
    //    let { State=elt; CostSoFar=costSoFar; CostAhead=costAhead } = (node |> List.head)
    //    heap |> BinomialHeapPQ.insert (costSoFar+costAhead) node

    let insert (ss:(int*(PathPart<'ELT> list)) SortedSet) (node:PathPart<'ELT> list) =
        let { State=elt; CostSoFar=costSoFar; CostAhead=costAhead } = (node |> List.head)
        //if ss.Add (costSoFar+costAhead,node) |> (=)false then failwith "huh" else
        ss.Add (costSoFar+costAhead,node)
        ss

    //let insertMany heap (sq: PathPart<'ELT> list seq) =
    //    sq
    //    |> Seq.fold (
    //        fun hp ({ State=elt; CostSoFar=costSoFar; CostAhead=costAhead}::lst) -> 
    //            BinomialHeapPQ.insert (costSoFar+costAhead) ({ State=elt; CostSoFar=costSoFar; CostAhead=costAhead}::lst) hp) heap

    let insertMany (ss:(int*(PathPart<'ELT> list)) SortedSet) (sq: PathPart<'ELT> list seq) =
        sq
        |> Seq.fold (
            fun _ ({ State=elt; CostSoFar=costSoFar; CostAhead=costAhead}::lst) -> 
                let newNode = { State=elt; CostSoFar=costSoFar; CostAhead=costAhead}::lst
                if ss.Add (costSoFar+costAhead, newNode) |> (=)false then failwith "huh" else ss
            ) ss
        |> ignore
                
        ss
             
    //let ofSeq (sq: PathPart<'ELT> list seq) =
    //    sq 
    //    |> Seq.filter (function | [] -> false | _ -> true)
    //    |> insertMany BinomialHeapPQ.empty   

    let ofSeq (sq: PathPart<'ELT> list seq) =
        
        sq 
        |> Seq.filter (function | [] -> false | _ -> true)
        |> insertMany (SortedSet<int*(PathPart<'ELT> list)>())

        
    //let dequeue heap =
    //    let elt = BinomialHeapPQ.getMin heap
    //    (elt, BinomialHeapPQ.deleteMin heap)


    let dequeue (ss:(int*(PathPart<'ELT> list)) SortedSet) =
        
        let elt = 
            if ss.Count=0 
            then None 
            else 
                let elt = ss.Min
                ss.Remove elt
                Some (snd elt)
        
        (elt, ss)
        //(elt, BinomialHeapPQ.deleteMin heap)