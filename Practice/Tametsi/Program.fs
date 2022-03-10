open System
open Aoc21_Common
open Aoc21_Common.Operators
//open FParsec
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Security.Cryptography
open System.Text
open AngouriMath.FSharp.Core
open AngouriMath.FSharp.Functions
open Microsoft.Z3
open Microsoft.Z3.Bool
open Microsoft.Z3.Int
open Microsoft.Z3.Real
open Microsoft.Z3.BitVec
open Microsoft.Z3.Array
open Microsoft.Z3.Addons


// Q=Question (Unknown)
// E=Empty
// F=Full
type LEVELTYPE = HEX | SQR
type CELLTYPE = Q | E of int option | F

[<EntryPoint>]
let main argv =    
    let inputFile = "l93.txt"

    let parseRowsCols str : (int*int) list = 
        if str="" then [] else
        str
        |> C.splitCh " " |> fun xs -> xs[1] 
        |> fun str -> str.ToCharArray() |> Array.toList
        |> List.map (
            function
            | '-' -> None
            | s -> Some (int (s.ToString()))
        )
        |> List.indexed
        |> List.filter (fun (idx,v) -> v <> None)
        |> List.map (fun (idx,Some v) -> idx,v)
        //|> Map.ofList
    
    let levelType, level, levelCols, levelRows = 
        let lns = C.readLines inputFile
        let levelType = match lns[0].ToLower() with | "hex" -> HEX | "square" | "sqr" -> SQR | _ -> failwith "huh"
        let levelCols = parseRowsCols lns[1]
        let levelRows = parseRowsCols lns[2]
            
        let level = 
            lns |> List.skip 3
            |> C.toPosMap (fun ln -> ln.ToCharArray())
            |> Map.toList
            |> List.choose (
                fun (pos,v) -> 
                    let cellV = 
                        match v with
                        | ' ' -> None
                        | '?' -> Some Q
                        | '-' -> Some <| E(None)
                        | 'x' -> Some <| F
                        | x -> Some <| E(x.ToString() |> int |> Some)
                        | _ -> failwith ""
                    match cellV with
                    | None -> None
                    | Some cv -> Some (pos,cv)
            )
        levelType, level, levelCols, levelRows

    0

    Gs.overrideContext(Dictionary<string,string>([KeyValuePair("proof","true")]))

    // no more than 10 bits necessary, that already covers 1024 cells 
    let zint = BitVec 10
    let zval = BitVecVal 10

    let ztrue = zval 1
    let zfalse = zval 0
    
    let pos2id (x,y) = x.ToString() + "-" + y.ToString()
    let cellsByPos = 
        level 
        |> List.map (fun (pos,celltype) -> 
            let nm = pos2id pos
            let v = pos |> pos2id |> zint
            pos,v
        )
        |> Map.ofList

    let globalNbrs lvl = lvl |>  List.choose (fun (pos,v) -> v |> function | E(Some ei) -> Some (pos,ei) | _ -> None)
    let globalRules lvl = 
        lvl 
        |> List.map (fun (pos, v) ->
            let cell = cellsByPos[pos] 
            if v=Q
            then
                (cell =. zfalse) ||. (cell =. ztrue)
            else
                let b = match v with | E(_) -> zfalse | _ -> ztrue
                cell =. b
            )
        |> List.toArray

    let getHexRules lvl =
        globalNbrs lvl, C.hexCoords_GetNeighbours, globalRules lvl

    let getSqrRules lvl =
        let globalR = globalRules lvl
        let columnR = 
            [| for (col,num) in levelCols do 
                let colSum =
                    cellsByPos.Keys
                    |> Seq.filter (fun (x,y) -> x=col)
                    |> Seq.map (fun pos -> cellsByPos[pos])
                    |> Array.ofSeq |> Array1D |> Array1D.SUM
                yield colSum =. (zval num)
            |]
        let rowR =
            [| for (row,num) in levelRows do 
                let colSum =
                    cellsByPos.Keys
                    |> Seq.filter (fun (x,y) -> y=row)
                    |> Seq.map (fun pos -> cellsByPos[pos])
                    |> Array.ofSeq |> Array1D |> Array1D.SUM
                yield colSum =. (zval num)
            |]
        //let rules = globalR ++ columnR ++ rowR
        let rules = globalR
        globalNbrs lvl, (fun pos -> C.MOVES_8DIR |> List.map ((+..) pos)), rules
            
        
    let nbrs, nbrFun, facts =
        match levelType with
        | HEX -> getHexRules level
        | SQR -> getSqrRules level

    let unknownsVars = level |> List.filter (fun (pos,v) -> v=Q) |> List.map (fun (pos,v) -> cellsByPos[pos])

    let nbrRules = 
        nbrs
        |> List.map (fun (pos, numN) ->
            nbrFun pos
            |> List.choose (cellsByPos.TryGetValue >> function | false,_ -> None | true,v -> Some v)
            //|> List.fold (+) (zval 0)
            |> List.toArray |> Array1D |> Array1D.SUM
            |> (=.) (zval numN)
        )
        |> List.toArray

    let opt = Opt()
    opt.AddAll (facts ++ nbrRules)
    //opt.Maximize (cellsByPos.Values |> Seq.reduce (+))  // keep as many cells unknown as possible - don't randomly invent cell values
    opt.CheckOrFail()

    let discoveredValuesCandidates = 
        unknownsVars
        |> List.map (fun unk -> unk.Expr.ToString().Trim('|'), (opt.Eval unk).ToString() |> int)

    let discoveredValues = 
        discoveredValuesCandidates
        |> List.filter (fun (name,v) -> 
            opt.Push()
            let pos = name |> C.splitCh "-" |> List.map int |> fun xs -> xs[0],xs[1]
            let cell = cellsByPos[pos]
            opt.Add (cell =. zval (1-v))
            // if opposite value is satisfiable, we don't want this candidate, and must return false
            let r = match opt.Check() with | Status.UNSATISFIABLE -> true | Status.UNKNOWN -> true | Status.SATISFIABLE -> false
            opt.Pop()
            r
        )
        |> List.sortBy (fun (pos,_) -> pos)


    
    //// this example made for l01.txt
    //// get proof of why (1,0) = zTrue , by:
    ////  .1. asserting the opposite
    ////  .2. letting z3 prove why the assumptions cannot all be true
    // let solver = 
    //    Z3.Solve (facts ++ nbrRules ++ [| cellsByPos[1,0] =. zfalse |])
    //    |> function | NoSolution(proof) -> failwith (proof.ToString()) | Unknown -> failwith "" | Solution(lst) -> lst

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


