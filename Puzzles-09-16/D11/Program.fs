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

// type State = { EL:int; }

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let lns = 
        C.readLines inputFile
        //|> List.indexed
        //|> List.map (
        //    fun (y, ln:string) -> 
        //        ln.ToCharArray() |> Array.map (fun ch -> int (ch.ToString())) |> List.ofArray |> List.indexed |> List.map (fun (x,n) -> ((x,y),n))
        //    )
        //|> List.concat

    let map0 = 
        lns 
        |> C.toPosMap (fun ln -> ln.ToCharArray() |> Array.map (fun ch -> int (ch.ToString())))
    0

    let neighbours (m:Map<(int*int),int>) (x,y) =
        //[(-1,0);(1,0);(0,-1);(0,1);(-1,1);(-1,-1);(1,-1);(1,1)]
        C.NEIGHBOURS_8DIR
        |> List.map (fun d -> (x,y) +.. d)
        |> List.filter (m.ContainsKey)
        |> List.filter (fun (x0,y0) -> m[(x0,y0)] > 0)

    let incNeighbours (m1:Map<(int*int),int>) ((x,y):int*int) : Map<(int*int),int> =
        let ns = neighbours m1 (x,y)
        let m2 = 
            ns
            |> List.fold (
                fun (m:Map<(int*int),int>) (nx,ny) -> 
                    let r = m.Add ((nx,ny),(m[nx,ny] + 1))
                    r
                ) m1
        m2.Add((x,y),0)


        

    let flashAll (m1:Map<(int*int),int>, numFlashes: int) =
        let nines = m1 |> Map.toList |> List.filter (fun (pos,v) -> v > 9) |> List.map fst
        if nines.Length=0 then None else

        let m2 = nines |> List.fold incNeighbours m1
        let f2 = numFlashes + nines.Length
        let r = (m2,f2)
        Some (r, r)


    let flashStep (m1:Map<(int*int),int>, numFlashes: int) _ =
        //let m2 =
        //    m1
        //    |> Map.toList
        //    |> List.map (fun (pos, n) -> (pos, n+1))
        //    |> Map.ofList
        let m2 = m1 |> Map.map (fun k v -> (v+1))

        let try3 = (m2, numFlashes) |> List.unfold flashAll |> List.tryLast
        if try3 = None then (m2,numFlashes) else try3.Value

    let r = 
        ([1..100])
        |> List.fold flashStep (map0, 0)

    let res1 = snd r

    let res2 = 
        Seq.initInfinite id
        |> Seq.scan flashStep (map0, 0)
        |> Seq.findIndex (fun (m, nf) -> m.Values |> Seq.forall ((=)0))
        //|>  (+)1

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


