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

//type State = { Node:string; }

type Block = L | R | BNum of int
type RecBlock = Num of int | RB of (RecBlock*RecBlock)

type ExplodeState = { RB:RecBlock; ExplodeToLeft: int option; ExplodeToRight: int option; Exploded: bool; }
type SplitState = { RB:RecBlock; Splitted: bool; }

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"

    let lns = 
        C.readLines inputFile
        |> List.map (
            fun (ln:string) -> 
                ln.ToCharArray() |> List.ofArray |> List.filter (fun ch -> ch <> ',') |> List.map (fun x -> x |> function | '[' -> L | ']' -> R | ch -> BNum(int(ch.ToString())))
            )
    0

    let rec toTree (b : Block list) =
        if b = [] then failwith "huh" else

        let (valL, rb) = 
            match b[0] with
            | L -> toTree b[1..]
            | BNum(i) -> (Num(i), b[1..])
            | _ -> failwith "huh"

        if rb = [] then (valL,[]) else

        let (valR, rb2) =
            match rb[0] with
            | L -> toTree rb[1..]
            | BNum(i) -> (Num(i), rb[1..])
            | _ -> failwith "huh"

        let vals = RB(valL, valR)
        (vals, rb2[1..])

    let trees = lns |> List.map toTree |> List.map fst

    0

    let rec doExplodeVal (es:ExplodeState):RecBlock =
        match es.RB, es.ExplodeToLeft, es.ExplodeToRight with
        | Num(i), Some v, _ -> Num(i+v)
        | Num(i), _, Some v -> Num(i+v)
        | RB(rbl,rbr),_,_ -> 
            let left = doExplodeVal {RB=rbl; ExplodeToRight=es.ExplodeToRight; ExplodeToLeft=None; Exploded=true }
            let right = doExplodeVal {RB=rbr; ExplodeToRight=None; ExplodeToLeft=es.ExplodeToLeft; Exploded=true }
            RB(left, right);
        | _, None, None -> es.RB

    let rec tryExplode (rb : RecBlock) (depth:int)  =
        match rb with
        | Num(_) -> 
            // failwith "huh"
            { RB=rb; ExplodeToLeft=None; ExplodeToRight = None; Exploded=false }
        | RB(Num(n1), Num(n2)) ->
            if depth <= 4 then { RB=rb; ExplodeToLeft=None; ExplodeToRight = None; Exploded=false } else
            { RB=Num(0); ExplodeToLeft=Some n1; ExplodeToRight= Some n2; Exploded=true }
            // ...

        | RB(rb1,rb2) ->
             let {RB=rbv1T; ExplodeToLeft=left1T; ExplodeToRight=right1T; Exploded=expl1T} = tryExplode rb1 (depth+1)
             let {RB=rbv2; ExplodeToLeft=left2; ExplodeToRight=right2; Exploded=expl2} = 
                match right1T with
                | Some i -> 
                    let rb22 = (doExplodeVal { RB=rb2; ExplodeToLeft=None; ExplodeToRight=right1T;Exploded=expl1T })
                    { RB=rb22; ExplodeToLeft=None;ExplodeToRight=None;Exploded=true }
                | None ->
                    if expl1T then ({ RB=rb2; ExplodeToLeft=None;ExplodeToRight=None;Exploded=true })
                    else tryExplode rb2 (depth+1)

             let {RB=rbv1; ExplodeToLeft=left1; ExplodeToRight=right1; Exploded=expl1} =
                match left2 with 
                | Some i2 ->
                    let rb12 = doExplodeVal { RB=rb1; ExplodeToLeft=left2; ExplodeToRight=None; Exploded=true; }
                    { RB=rb12; ExplodeToLeft=None;ExplodeToRight=None;Exploded=true }
                | None -> {RB=rbv1T; ExplodeToLeft=left1T; ExplodeToRight=right1T; Exploded=expl1T} 

             {RB= RB(rbv1,rbv2); ExplodeToLeft = left1; ExplodeToRight=right2; Exploded=expl1||expl2}
            
        

    let rec trySplit (rb :RecBlock) =
        match rb with
        | Num(x) -> 
            if x < 10 then { RB=rb; Splitted=false; } else
            let numL = x/2
            let numR = x-numL
            { RB=RB(Num(numL),Num(numR)); Splitted=true; }
            
        | RB(rbL,rbR) ->
            let splitL = trySplit rbL
            if splitL.Splitted then { RB=RB(splitL.RB, rbR); Splitted=true } else
            let splitR = trySplit rbR
            { RB=RB(splitL.RB, splitR.RB); Splitted=splitR.Splitted }
            
        

            

    let reduceSnailfish (tree :RecBlock) =
        let expl = tryExplode tree 1
        if expl.Exploded then Some (expl.RB, expl.RB) else
        
        let spl = trySplit tree
        if spl.Splitted then Some (spl.RB, spl.RB) else
        None

    //let reducedTrees = 
    //    trees 
    //    |> List.map (fun tree ->
    //        let reducedTree = tree |> Seq.unfold reduceSnailfish |> C.prepend [tree] |> Seq.last
    //        reducedTree
    //    )

    let sumSnailfish (tree1:RecBlock) (tree2:RecBlock) =
        let rb = RB(tree1,tree2)
        rb |> Seq.unfold reduceSnailfish |> C.prepend [rb] |> Seq.last
        //reduceSnailfish rb

    let summation = 
        trees
        |> Seq.skip 1
        |> Seq.fold sumSnailfish (Seq.head trees)

    let rec magnitude (tree:RecBlock) =
        match tree with
        | Num(y) -> y
        | RB(x1,x2) ->
            let y1 = magnitude x1
            let y2 = magnitude x2

            3*y1 + 2*y2

    let res1 = magnitude summation

    let res2 = 
        List.allPairs trees trees
        |> List.filter (fun (x,y) -> x <> y)
        |> List.map (fun (x,y) -> sumSnailfish x y)
        |> List.maxBy magnitude
        |> magnitude

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


