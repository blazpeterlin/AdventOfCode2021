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
//let internal (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
//let internal (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)


let internal (+.) (x:int option,y:int) = if x=None then None else Some (x.Value+y)
let internal (-.) (x:int option,y:int) = if x=None then None else Some (x.Value-y)



//type State = { Node:string; }

type CubeModel = { MinX: int option; MaxX: int option; MinY: int option; MaxY: int option; MinZ: int option; MaxZ: int option; Val: bool option }

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let lns = 
        C.readLines inputFile
        |> List.map (
            fun (ln:string) -> 
                ln |> C.splitCh " xyz=..," 
                |> fun xs -> ((if xs[0]="on" then true else false), xs |>List.skip 1 |> List.map int)
            )
    0
    
    let MIN (y:int,x:int option) = if x=None then y else min x.Value y
    let MAX (y:int,x:int option) = if x=None then y else max x.Value y

    let cubes = 
        //List.allPairs [-50..50] [-50..50]
        //|> List.allPairs [-50..50]
        //|> List.map (fun (x,(y,z)) -> (x,y,z),false)
        //|> Map.ofList
        seq { for x in [-50..50] do for y in [-50..50] do for z in [-50..50] do yield (x,y,z),false}
        |> Map.ofSeq
    

    // simpler solution for part 1
    let applyRule (cbs:Map<int*int*int,bool>) (newVal:bool, ln:int list) =
        let minx,maxx,miny,maxy,minz,maxz = ln[0],ln[1],ln[2],ln[3],ln[4],ln[5]
        cbs.Keys 
        |> seq 
        |> Seq.filter (fun(x,y,z) -> x >= minx && x <= maxx && y >= miny && y <= maxy && z >= minz && z <= maxz)
        |> Seq.map (fun (x,y,z) -> (x,y,z),newVal)
        |> Seq.fold (fun c (k,v) -> c |> Map.add k v) cbs
    let final = 
        lns
        |> List.fold applyRule cubes
    let res1 = final |> Map.toList |> List.filter(fun (_,v) -> v=true) |> List.length


    let st0 = [{ MinX=None; MaxX=None; MinY=None; MaxY=None; MinZ=None; MaxZ=None; Val=None; }]

    let isFeasibleCube (cube:CubeModel) =   
        (cube.MinX=None || cube.MaxX=None || cube.MinX <= cube.MaxX)
        && (cube.MinY=None || cube.MaxY=None || cube.MinY <= cube.MaxY)
        && (cube.MinZ=None || cube.MaxZ=None || cube.MinZ <= cube.MaxZ)

    let applyToCube (newVal:bool, ln:int list) (cube:CubeModel)  =
        let minx,maxx,miny,maxy,minz,maxz = ln[0],ln[1],ln[2],ln[3],ln[4],ln[5]
        let isFeasibleIntersect = 
            (cube.MaxX=None || cube.MaxX.Value >= minx)
            && (cube.MaxY=None || cube.MaxY.Value >= miny)
            && (cube.MaxZ=None || cube.MaxZ.Value >= minz)
            && (cube.MinX=None || cube.MinX.Value <= maxx)
            && (cube.MinY=None || cube.MinY.Value <= maxy)
            && (cube.MinZ=None || cube.MinZ.Value <= maxz)
        if not isFeasibleIntersect then [cube] else

        let iscMinX= MAX(minx,cube.MinX)
        let iscMinY= MAX(miny,cube.MinY)
        let iscMinZ= MAX(minz,cube.MinZ)
        let iscMaxX= MIN(maxx,cube.MaxX)
        let iscMaxY= MIN(maxy,cube.MaxY)
        let iscMaxZ= MIN(maxz,cube.MaxZ)

        let outsiderL = { MinX=cube.MinX; MaxX = Some(iscMinX-1); MinY=cube.MinY;MaxY=cube.MaxY;MinZ=cube.MinZ;MaxZ=cube.MaxZ; Val=cube.Val }
        let outsiderR = { MinX=Some(iscMaxX+1); MaxX = cube.MaxX; MinY=cube.MinY;MaxY=cube.MaxY;MinZ=cube.MinZ;MaxZ=cube.MaxZ; Val=cube.Val }
        let outsiderT = { MinX=Some(iscMinX); MaxX=Some(iscMaxX); MinY=cube.MinY;MaxY=cube.MaxY; MinZ=Some(iscMaxZ+1);MaxZ=cube.MaxZ; Val=cube.Val }
        let outsiderB = { MinX=Some(iscMinX); MaxX=Some(iscMaxX); MinY=cube.MinY;MaxY=cube.MaxY; MinZ=cube.MinZ; MaxZ=Some(iscMinZ-1); Val=cube.Val }
        let outsiderA = { MinX=Some(iscMinX); MaxX=Some(iscMaxX); MinY=Some(iscMaxY+1);MaxY=cube.MaxY; MinZ=Some(iscMinZ); MaxZ=Some(iscMaxZ); Val=cube.Val }
        let outsiderC = { MinX=Some(iscMinX); MaxX=Some(iscMaxX); MinY=cube.MinY;MaxY=Some(iscMinY-1); MinZ=Some(iscMinZ); MaxZ=Some(iscMaxZ); Val=cube.Val }
        let insider = { MinX=Some(iscMinX);MaxX=Some(iscMaxX); MinY=Some(iscMinY);MaxY=Some(iscMaxY); MinZ=Some(iscMinZ); MaxZ=Some(iscMaxZ); Val=Some newVal }

        let feasibleCubes = 
            [outsiderL;outsiderR;outsiderT;outsiderB;outsiderA;outsiderC;insider]
            |> List.filter isFeasibleCube

        feasibleCubes

    let applyToCubeList (cubes:CubeModel list) lnPair  =
        cubes |> List.map (applyToCube lnPair) |> List.concat

    let finalCubes = 
        lns
        |> List.fold applyToCubeList st0

    let isNotInfinite cube =
        cube.MinX <> None && cube.MaxX <> None
        && cube.MinY <> None && cube.MaxY <> None
        && cube.MinZ <> None && cube.MaxZ <> None

    let finalNonInfiniteCubes = finalCubes |> List.filter isNotInfinite
    let positiveCubes = finalNonInfiniteCubes |> List.filter( fun cb -> cb.Val=Some true)
    let numCubes = 
        positiveCubes 
        |> List.map (fun cb -> int64(cb.MaxX.Value-cb.MinX.Value+1)*int64(cb.MaxY.Value-cb.MinY.Value+1)*int64(cb.MaxZ.Value-cb.MinZ.Value+1))
        |> List.sum

    let res2 = numCubes

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


