open System
open Aoc21_Common
open FParsec
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Security.Cryptography
open System.Text
open Microsoft.Z3
open Microsoft.Z3.Bool
open Microsoft.Z3.Int
open Microsoft.Z3.Real
open Microsoft.Z3.Array
open Microsoft.Z3.Function
open Microsoft.Z3.Api

type ENV = T | P
let internal (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
let internal (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)

//type State = { Node:string; }

type AMPHTYPE = A|B|C|D
type BLOCK = AMPH of AMPHTYPE|WALL|SPACE

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"

    let parse file = 
        C.readLines file
        |> List.map (
            fun (ln:string) -> 
                ln.ToCharArray() |> List.ofArray|> List.map(fun ch -> ch |> function | 'A'->AMPH A|'B'->AMPH(B)|'C'->AMPH(C)|'D'->AMPH(D)|'.'|' '->SPACE|'#'->WALL)
            )
        |> List.indexed
        |> List.map (fun (y,blocks) -> blocks |> List.indexed |> List.map (fun (x,b) -> ((x,y),b)))
        |> List.concat
        |> List.filter(fun (pos,v) -> v <> WALL && v <> SPACE) // both became implicit
        |> Map.ofList

    let calcTarget (dimx:int,dimy:int) =
        [ for x in [3;5;7;9] do for y in 2..dimy do yield (x,y),match x with | 3 -> AMPH(A) | 5 -> AMPH(B) | 7 -> AMPH(C) | 9 -> AMPH(D)]
        |> Map.ofList
    
    let getTargetX = function | AMPH(A) -> 3 | AMPH(B) -> 5 | AMPH(C) -> 7 | AMPH(D) -> 9

    let getCost amph =
        amph 
        |> function 
            | WALL | SPACE -> failwith "huh"
            | AMPH(A) -> 1
            | AMPH(B) -> 10
            | AMPH(C) -> 100
            | AMPH(D) -> 1000
    
    let getPathLength (st:Map<int*int,BLOCK>) (x0,y0) (x1,y1) =
        let minx = min x0 x1
        let maxx = max x0 x1
        let condition1 = 
            seq { for x in minx..maxx do yield (x,y1) }
            |> Seq.forall (fun pos -> st.ContainsKey(pos) |> not)
        let condition2 = 
            seq { for y in 2..y0-1 do yield (x0,y)}
            |> Seq.forall (fun pos -> st.ContainsKey(pos) |> not)
        if condition1 && condition2
        then Some(abs(x0-x1) + abs(y0-y1))
        else None
        
    let badPos = [3,1;5,1;7,1;9,1]

    let allPos = 
        [ for x in 1..11 do yield x,1 ]
        |> List.append [ for y in 2..5 do yield! [3,y;5,y;7,y;9,y]]
        |> List.except badPos
        //|> List.indexed
    let indexedPosCount = allPos |> List.length |> int64
    
    let doActionsGeneric (dimx,dimy) (stN:Map<int*int,BLOCK>) (st:Map<int*int,BLOCK>) =
        let usefulSt = st |> Map.toList
       
        let remainingAmphs = 
            usefulSt 
            |> Seq.filter (fun((x,y),amph) -> 
                    (stN |> Map.tryFind (x,y) |> function | None -> true | Some v -> v<>amph)
                    || (
                        y<dimy
                        && seq { y+1..dimy } |> Seq.exists (fun iy -> match st |> Map.tryFind (x,iy) with | None -> true | Some v -> v <> amph)
                    )
                )
            |> Seq.toList

        let clearAmp = 
            remainingAmphs
            |> List.filter (fun ((x,y),amph) ->
                    let targetX = getTargetX amph
                    let isClearGoal = 
                        //[targetX,2;targetX,3]
                        [ for iy in 2..dimy do yield targetX,iy ]
                        |> List.choose (fun pos -> st |> Map.tryFind pos)
                        |> List.except [amph]
                        |> List.isEmpty
                    let minx =  min targetX x
                    let maxx = max targetX x
                    let isClearTop = [minx..maxx] |> List.map (fun x -> (x,1)) |> List.choose (fun pos -> st |> Map.tryFind pos) |> List.except [amph] |> List.isEmpty
                    let isClearToTop = seq { for iy in 2..y-1 do yield x,iy } |> Seq.forall (fun pos -> st.ContainsKey(pos) |> not)
                    isClearGoal && isClearTop && isClearToTop
                )
            |> List.tryHead

        //let clearAmp = None

        match clearAmp with
        | Some((x,y),amph) ->
            let targetX = getTargetX amph
            let targetY = [ for iy in dimy.. -1..2 do yield targetX,iy] |> List.find (fun pos -> st.ContainsKey(pos) |> not) |> snd
            let pathLength =
                if x=targetX then
                    abs (y-targetY)
                else
                    abs (x-targetX) + abs(y-1) + abs(targetY-1)
            let cost = pathLength * (getCost amph)
            let nextSt = st |> Map.add (targetX,targetY) amph |> Map.remove (x,y)
            0
            seq { nextSt,cost }
        | None ->
            let r =
                remainingAmphs
                |> Seq.filter (fun ((x,y),amph) -> y>1)
                |> Seq.map (fun ((x,y),amph) -> 
                        let cost = getCost amph

                        let allFreeSpacesUp = 
                            seq { for x in 1..dimx+2 do yield (x,1) }
                            |> Seq.except badPos
                            |> Seq.choose (fun targetPos ->
                                match getPathLength st (x,y) targetPos with
                                | None -> None
                                | Some res -> Some(targetPos,res))
                            |> List.ofSeq

                        allFreeSpacesUp 
                        |>List.map (fun (newPos,len) -> 
                            let newSt = st|> Map.add newPos amph |> Map.remove (x,y)
                            newSt,cost*len
                            )
                        |>Seq.ofList
                    )
                |> Seq.concat
            r
    
    let heuristicGeneric (dimx,dimy) (stN:Map<int*int,BLOCK>) st = 
        0
        //let totalH = 
        //    st 
        //    |> Map.toSeq 
        //    |> Seq.map (fun ((x,y),amph) -> 
        //        let fits,bottomFits = 
        //            let fits = stN |> Map.tryFind (x,y) |> function | None -> false | Some v -> v=amph 
        //            if not fits then false,false else
        //            let bottomFits = [ for iy in y+1..dimy do yield x,iy ] |> List.forall (fun pos -> st[pos]=amph)
        //            if not bottomFits then false,true else true,true

        //        if fits && bottomFits then 0 else
        //        let cost = getCost amph
        //        if fits then y*2*cost else
        //        let targetX = getTargetX amph
        //        let moveUpLen = y-1
        //        let moveSidewaysLen = abs(x-targetX)
        //        let moveDownLen = seq { dimy.. -1..2 } |> Seq.filter (fun iy -> st |> Map.tryFind (targetX,iy) |> (<>) (Some amph)) |> Seq.head |> fun iy -> iy/2
        //        cost * (moveUpLen+moveSidewaysLen+moveDownLen)
        //    )
        //    |> Seq.sum
        //totalH

    let calcRes (st0:Map<int*int,BLOCK>) =
        
        let dimx = st0 |> Map.toSeq |> Seq.map (fun ((x,y),v) -> x) |> Seq.max
        let dimy = st0 |> Map.toSeq |> Seq.map (fun ((x,y),v) -> y) |> Seq.max
        let dims = dimx,dimy
        let stN = calcTarget dims

        let h = heuristicGeneric dims stN
        let doActions = doActionsGeneric dims stN


        // doesn't seem to help much?
        let map2state (m:Map<int*int,BLOCK>) : int64 =
            allPos 
            |> Seq.map (fun pos -> match Map.tryFind pos m with | None -> 0L | Some(AMPH(A)) -> 1L |  Some(AMPH(B)) -> 2L | Some(AMPH(C))-> 3L |  Some(AMPH(D))-> 4L | _ -> failwith "huh")
            |> Seq.fold (fun a b -> a*5L+b) 0L
            //m |> Map.toSeq |> Seq.sortBy snd |> Seq.map fst |> Seq.map (fun pos -> indexedPos[pos]) |> Seq.fold (fun a b -> a*indexedPosCount+(int64 b)) 0L

        let r =
            st0
            |> AStar.unfold true doActions h map2state (fun x -> x.State = stN)
            |> Seq.filter (fun x -> x.IsGoal)
            |> Seq.head

        r.TotalCost

    
    let lns1 = parse inputFile

    let lns2 = 
        lns1
        |> Map.toList
        |> List.map (fun ((x,y),pos) -> if y < 3 then (x,y),pos else (x,y+2),pos)
        |> List.append [
            (3,3),AMPH(D);(5,3),AMPH(C);(7,3),AMPH(B);(9,3),AMPH(A)
            (3,4),AMPH(D);(5,4),AMPH(B);(7,4),AMPH(A);(9,4),AMPH(C)
        ]
        |> Map.ofList
            


    // 15160 after 0:03
    let res1 = calcRes lns1

    // 46772 after 0:25
    let res2 = calcRes lns2

    0    


    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


