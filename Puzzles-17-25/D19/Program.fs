open System
open Aoc21_Common
open Aoc21_Common.Operators
open FParsec
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Security.Cryptography
open System.Text

type ENV = T | P

//type State = { Node:string; }

[<EntryPoint>]
let main argv =    
    let env = T
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"

    let lns = 
        C.readLines inputFile
        |> List.map (
            fun (ln:string) -> 
                ln
            )
    0

    let zeroesIdx = lns |> List.indexed |> List.filter( fun (x,y) -> y="") |> List.map fst |>C.prepend [-1] |> List.ofSeq

    let scannerChunks = 
        lns 
        |> List.mapi (fun idx ln -> 
            let zidx = zeroesIdx |> List.filter(fun z -> z < idx) |> List.max
            (ln,zidx)
            )
        |> List.groupBy snd |> List.map snd
        //|> List.map (fun chnk ->chnk |> List.rev |> List.skip 1 |> List.rev |> List.skip 1)
        |> List.map (fun chnk -> chnk |> List.map fst |> List.filter(fun str-> str<>"" && not(str.StartsWith "---")))
        |> List.map (fun chnk ->
            chnk
            |> List.map (fun s -> s |> C.splitCh "," |> List.map int |> fun xs -> (xs[0],xs[1],xs[2]))
            //|> Set.ofList
        )

    
    let fsInvert = 
        [
            fun (x,y,z) -> (x,y,z);
            fun (x,y,z) -> (x,y,-z);
            fun (x,y,z) -> (x,-y,z);
            fun (x,y,z) -> (x,-y,-z);
            fun (x,y,z) -> (-x,y,z);
            fun (x,y,z) -> (-x,y,-z);
            fun (x,y,z) -> (-x,-y,z);
            fun (x,y,z) -> (-x,-y,-z);
        ]

    let fsRotate = 
        [
            fun (x,y,z) -> (x,y,z);
            fun (x,y,z) -> (x,z,y);
            fun (x,y,z) -> (y,x,z);
            fun (x,y,z) -> (y,z,x);
            fun (x,y,z) -> (z,x,y);
            fun (x,y,z) -> (z,y,x);
        ]

    let allTransformations = 
        List.allPairs fsInvert fsRotate 
        |> List.map (fun (f1,f2) -> f1 >> f2)
        |> List.filter(fun f -> 
            // checks if this rotation is ok, through cross product

            let (x0,x1,x2) = f(1,0,0)
            let (y0,y1,y2) = f(0,1,0)
            let (z0,z1,z2) = f(0,0,1)

            let (w0,w1,w2) = (
                x1*y2-x2*y1,
                x2*y0-x0*y2,
                x0*y1-x1*y0
                )
            (w0,w1,w2) = (z0,z1,z2)
        )

    
    let getAllTransformationsOfScannerChunk (cc : (int*int*int) list) (*: list<(int*int*int)> list *)=

        let r = allTransformations |> List.map (fun f -> (f, cc |> List.map (fun c -> f c)))
        r

    let tryFindOffsetMapping (cc1 : (int*int*int) list) (cc2 : (int*int*int) list) =
        let goodOffsetMapping = 
            List.allPairs cc1 cc2
            |> List.map (fun ((x0,y0,z0),(x1,y1,z1)) -> (x0-x1,y0-y1,z0-z1))
            |> List.countBy id
            |> List.sortByDescending snd
            |> List.head 
            |> fun ((ox,oy,oz), highestCount) -> if highestCount >= 12 then Some (fun (x,y,z)->(x+ox,y+oy,z+oz)) else None
        goodOffsetMapping
        
    let tryFindRotationMapping (cc1 : (int*int*int) list) (cc2 : (int*int*int) list) =
        let allTransformationsCC2 = getAllTransformationsOfScannerChunk cc2
        let goodTransformation = 
            allTransformationsCC2
            |> List.choose (fun (invRotation, icc2) -> 
                match tryFindOffsetMapping cc1 icc2 with
                | None -> None
                | Some translation -> Some (invRotation >> translation)
            )
            |> List.tryHead
        
        goodTransformation


    let findAndTransformNextScanner (mappedScanners,unmappedScanners) =
        if unmappedScanners = [] then None else

        let getTransformation unmappedScChunk = 
            mappedScanners
            |> List.choose (fun ((f1:(int*int*int->int*int*int),mappedScChunk)) -> 
                match tryFindRotationMapping mappedScChunk unmappedScChunk with
                | None -> None
                | Some f2 -> Some (f2 >> f1)
            )

        let (foundScanner, foundMapping) = 
            unmappedScanners
            |> List.find (fun scChunkCandidate -> 
                let gts = (getTransformation scChunkCandidate)
                gts.Length <> 0
                )
            |> fun chnk -> (chnk, (getTransformation chnk) |> List.head)
            
        let newMappingAndScanner = (foundMapping, foundScanner)
        let a = 1

        let nextMappedScanners = newMappingAndScanner::mappedScanners
        let nextUnmappedScanners = unmappedScanners |> List.except [foundScanner]

        let r = (nextMappedScanners, nextUnmappedScanners)

        Some (r, r)

    let fid (x,y,z) = (x,y,z)
    let st0 = [(fid,scannerChunks[0])], (scannerChunks |> List.skip 1)
    let allTransformationsChunks = 
        st0 |> List.unfold findAndTransformNextScanner |> List.last |> fst

    let allCoordsMappedToFirstScanner =
        allTransformationsChunks
        |> List.map (fun (f,chnk) -> chnk |> List.map f)
        |> List.concat
        |> List.distinct


    let res1 = allCoordsMappedToFirstScanner |> List.length

    let pos0mappedByAllScanners =
        allTransformationsChunks
        |> List.map (fun (f,chnk) -> f (0,0,0))

    let manhattanDist ((x1,y1,z1),(x2,y2,z2)) = abs (x1-x2) + abs(y1-y2) + abs(z1-z2)

    let res2 = 
        List.allPairs pos0mappedByAllScanners pos0mappedByAllScanners
        |> List.map manhattanDist
        |> List.max

    //let chk1 = allCoords |>List.map (fun (x,y,z) -> ""+x.ToString()+","+y.ToString()+","+z.ToString()) |> List.reduce (fun x y -> x + Environment.NewLine + y)

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


