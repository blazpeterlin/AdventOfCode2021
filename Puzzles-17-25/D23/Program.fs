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
        |> List.filter(fun (pos,v) -> v <> WALL) // implicit
        |> Map.ofList
        |> Map.add (-1,-1) SPACE

    let calcTarget st0 =
        st0
        |> Map.toList
        |> List.map (fun ((x,y),v) -> 
            let newV = 
                match v,x with
                | SPACE,_ -> SPACE
                | AMPH(amph),3 -> AMPH(A)
                | AMPH(amph),5 -> AMPH(B)
                | AMPH(amph),7 -> AMPH(C)
                | AMPH(amph),9 -> AMPH(D)
                | _ -> failwith "huh"
            (x,y),newV
            )
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
            |> Seq.forall (fun pos -> st[pos]=SPACE)
        let condition2 = 
            seq { for y in 2..y0-1 do yield (x0,y)}
            |> Seq.forall (fun pos -> st[pos]=SPACE)
        if condition1 && condition2
        then Some(abs(x0-x1) + abs(y0-y1))
        else None

    
    let doActionsGeneric dimx dimy (stN:Map<int*int,BLOCK>) (st:Map<int*int,BLOCK>) =
        let usefulSt = 
            st 
            |> Map.toSeq 
            |> Seq.filter (fun ((x,y),amph) -> amph <> SPACE) 
            |> List.ofSeq
        let tempPos = [3,1;5,1;7,1;9,1]
       
        let remainingAmphs = 
            usefulSt 
            |> Seq.filter (fun((x,y),amph) -> 
                    stN[(x,y)]<>amph
                    || (
                        y<dimy
                        && seq { y+1..dimy } |> Seq.exists (fun iy -> match st |> Map.tryFind (x,iy) with | None -> false | Some v -> v <> amph)
                    )
                )
            |> Seq.toList

        let remainingAmphsBadPos = 
            remainingAmphs |> List.filter (fun (pos,amph) -> tempPos |> List.contains pos)

        let remainingAmphs = if remainingAmphsBadPos.Length>0 then remainingAmphsBadPos else remainingAmphs

        let clearAmp = 
            remainingAmphs
            |> List.filter (fun ((x,y),amph) ->
                    let targetX = getTargetX amph
                    let isClearGoal = 
                        //[targetX,2;targetX,3]
                        [ for iy in 2..dimy do yield targetX,iy ]
                        |> List.choose (fun pos -> st |> Map.tryFind pos)
                        |> List.except [amph;SPACE]
                        |> List.isEmpty
                    let minx =  min targetX x
                    let maxx = max targetX x
                    let isClearTop = [minx..maxx] |> List.map (fun x -> (x,1)) |> List.map (fun pos -> st[pos]) |> List.except [SPACE;amph] |> List.isEmpty
                    let isClearToTop = seq { for iy in 2..y-1 do yield x,iy } |> Seq.forall (fun pos -> st[pos]=SPACE)
                    isClearGoal && isClearTop && isClearToTop
                )
            |> List.tryHead

        //let clearAmp = None

        match clearAmp with
        | Some((x,y),amph) ->
            let targetX = getTargetX amph
            let targetY = [ for iy in dimy.. -1..2 do yield targetX,iy] |> List.find (fun pos -> st[pos]=SPACE) |> snd
            let pathLength =
                if x=targetX then
                    abs (y-targetY)
                else
                    abs (x-targetX) + abs(y-1) + abs(targetY-1)
            let cost = pathLength * (getCost amph)
            let nextSt = st |> Map.add (targetX,targetY) amph |> Map.add (x,y) SPACE
            0
            seq { nextSt,cost }
        | None ->
            let r =
                remainingAmphs
                |> Seq.filter (fun ((x,y),amph) -> y>1)
                |> Seq.map 
                    (fun ((x,y),amph) -> 
                        let cost = getCost amph

                        let allFreeSpacesUp = 
                            seq { for x in 1..dimx do yield (x,1) }
                            |> Seq.choose (fun targetPos ->
                                match getPathLength st (x,y) targetPos with
                                | None -> None
                                | Some res -> Some(targetPos,res))
                            |> List.ofSeq

                        allFreeSpacesUp 
                        |>List.map (fun (newPos,len) -> 
                            let newSt = st|> Map.add newPos amph |> Map.add (x,y) SPACE
                            newSt,cost*len
                            )
                        |>Seq.ofList
                    )
                    |> Seq.concat
            r
    
    let heuristicGeneric dimx dimy (stN:Map<int*int,BLOCK>) st = 
       
        let totalH = 
            st 
            |> Map.toSeq 
            |> Seq.filter (fun ((x,y),amph) -> amph <> WALL && amph <> SPACE)
            |> Seq.map (fun ((x,y),amph) -> 
                let isOnResult = 
                    let fits = stN[x,y]=amph 
                    if not fits then false else
                    let bottomFits = [ for iy in y+1..dimy do yield x,iy ] |> List.forall (fun pos -> st[pos]=amph)
                    if not bottomFits then false else true

                if isOnResult
                then
                    0
                else
                    1 * (getCost amph)
                )
            |> Seq.sum
        totalH

    let calcRes st0 =
        let stN = calcTarget st0
        
        let dimx = st0.Keys |> Seq.map (fun (x,y) -> x) |> Seq.max
        let dimy = st0.Keys |> Seq.map (fun (x,y) -> y) |> Seq.max |> fun y -> y-1
    




        let h = heuristicGeneric dimx dimy stN
        let doActions = doActionsGeneric dimx dimy stN

        let r =
            st0
            |> AStar.unfold true doActions h id (fun x -> x.State = stN)
            |> Seq.filter (fun x -> x.IsGoal)
            |> Seq.head

        r.TotalCost
        // 19176 too high

        // 15160 after 2:10

    
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
            


    // 15160 after 0:07
    let res1 = calcRes lns1

    // 46772 after 1:43
    let res2 = calcRes lns2

        
    0
    

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code















    //if tempPos |> List.contains lastMovedTo
           //then
           //    let amph = st[lastMovedTo]
           //    let (x,y) = lastMovedTo
           //    let cost = getCost amph
           //    let neighbours = 
           //        moves
           //        |> List.map ((+..)lastMovedTo) 
           //        |> List.filter (fun pos -> st[pos]=SPACE)
           //        |> List.filter (fun (x2,y2) -> 
           //            y2 <= y || 
           //            (
           //                stN[x2,y2]=amph
           //                && (stN[x2,y2+1]=WALL || stN[x2,y2+1]=amph)
           //            ))

           //    neighbours 
           //    |>List.map (fun newPos -> 
           //        let newSt = st|> Map.add newPos amph |> Map.add lastMovedTo SPACE
           //        (newSt,newPos,forceToRoom),cost
           //        )
           //    |>Seq.ofList
           ////elif forceToRoom
           ////then
           ////    let amph = st[lastMovedTo]
           ////    let cost = getCost amph
           ////    let (x,y) = lastMovedTo

           ////    let targetX = getTargetX amph
           ////    if [st[targetX,2];st[targetX,3]] |> List.except [SPACE;amph] |> List.isEmpty |> not then Seq.empty else

           ////    let neighbours = 
           ////        moves 
           ////        |> List.map ((+..)(x,y)) 
           ////        |> List.filter (fun pos -> st[pos]=SPACE)
           ////        |> List.filter (fun (x2,y2) -> 
           ////            y2 <= y || 
           ////            (
           ////                stN[x2,y2]=amph
           ////                && (stN[x2,y2+1]=WALL || stN[x2,y2+1]=amph || stN[x2,y2+1]=SPACE)
           ////            )
           ////            )
           ////    neighbours 
           ////    |>List.map (fun newPos -> 
           ////        let newSt = st|> Map.add newPos amph |> Map.add lastMovedTo SPACE
           ////        let forceToRoom2 = (newPos |> snd) = 1
           ////        (newSt,newPos,forceToRoom2),cost
           ////        )

           ////    |>Seq.ofList
           //else