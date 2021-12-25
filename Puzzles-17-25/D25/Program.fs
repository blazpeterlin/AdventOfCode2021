open System
open Aoc21_Common
open Aoc21_Common.Operators
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
open Microsoft.Z3.Addons

type ENV = T | P

//type State = { Node:string; }

[<EntryPoint>]
let main argv =    
    let env = P
    
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"

    let parse file = 
        C.readLines file
        |> C.toPosMap (fun ln -> ln.ToCharArray())
        
    let st0 = parse inputFile
    0

    let maxx = st0.Keys |> Seq.map (fst) |> Seq.max
    let lenx = maxx+1
    let maxy = st0.Keys |> Seq.map (snd) |> Seq.max
    let leny = maxy+1

    let stepForOneDir (dir:char) (st:Map<int*int,char>) = 
        let changedLocs = 
                  st
                  |> Map.toSeq
                  |> Seq.filter (fun ((x,y),v) -> v=dir)
                  |> Seq.map (fun ((x,y),v) -> 
                      let v = st[x,y]

                      let nextPos = 
                          match v with
                          | '.' -> x,y
                          | '>' -> (x+1)%lenx,y
                          | 'v' -> x,(y+1)%leny

                      if st[nextPos] <> '.' then [] else
                      [(nextPos,v);(x,y),'.']
                      )
                  |> Seq.concat

        let changedMap = changedLocs |> Seq.fold (fun m (k,v) -> m |> Map.add k v) st
        changedMap
    
    let step (st:Map<int*int,char>) = 
        let r = st |> stepForOneDir '>' |> stepForOneDir 'v'

        if r=st then None
        else Some(r,r)
    0

    let res1 = st0 |> Seq.unfold step |> Seq.length |> (+)1

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


