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

// type State = { EL:int; }
type Operator = PLUS | MINUS | TIMES | DIV
type Parenthesis = LEFT | RIGHT
type Lex = Num of int | Op of Operator | Pnt of Parenthesis

type LexState = { Idx:int; ReadingNum:string; }

[<EntryPoint>]
let main argv =    
    let env = T
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable ln = 
        C.readLines inputFile
        |> Seq.head
        //|> Seq.head
        //|> List.map (fun x -> x |> C.splitCh "[]")
        //|> C.parseList id
        //|> C.parseMatrix id
    0

    

    let str2lex { Idx=Idx; ReadingNum=ReadingNum; } : (Lex option*LexState) option =
        if Idx>=ln.Length 
        then 
            if ReadingNum.Length > 0 
            then 
                let nextSt = { Idx=Idx; ReadingNum="" }
                Some(Some(Num(int ReadingNum)), nextSt)
            else None
        else

        let ch = ln[Idx]
        let next = {Idx=Idx+1; ReadingNum=ReadingNum}
        match ch, ReadingNum with
        | c, rn when Char.IsNumber(c) -> Some(None, { next with ReadingNum=rn+c.ToString() })
        | c, rn when not( Char.IsNumber(c)) && rn.Length > 0 -> Some(Some (Num(int rn)), { Idx=Idx; ReadingNum=""; })
        | '+', _ -> Some(Some(Op(PLUS)), next)
        | '-', _ -> Some(Some(Op(MINUS)), next)
        | '*', _ -> Some(Some(Op(TIMES)), next)
        | '/', _ -> Some(Some(Op(DIV)), next)
        | '(', _ -> Some(Some(Pnt(LEFT)), next)
        | ')', _ -> Some(Some(Pnt(RIGHT)), next)
        | _ -> failwith "unexpected"

    let lex0 = { Idx=0; ReadingNum=""; } 
    let lexN = lex0 |> Seq.unfold str2lex |> Seq.choose id |> List.ofSeq
    0
    let matchPrior1 (x,y,z) =
        match x,y,z with 
        | Num(xx), Op(TIMES), Num(zz) -> Num(xx*zz) |>Some
        | Num(xx), Op(DIV), Num(zz) -> Num(xx/zz) |>Some
        | _ -> None

    let matchPrior2 (x,y,z) =
        match x,y,z with 
        | Num(xx), Op(PLUS), Num(zz) -> Num(xx+zz) |>Some
        | Num(xx), Op(MINUS), Num(zz) -> Num(xx-zz) |>Some
        | _ -> None

    let matchPrior3 (x,y,z) =
        match x,y,z with 
        | Pnt(LEFT), Num(yy), Pnt(RIGHT) -> Num(yy) |>Some
        | _ -> None
        
    let evalWithMatcher matcher (tkns:Lex list)  =
        if tkns.Length <= 2 then None else
        0   
        let firstMatch =
            tkns
            |> List.windowed 3
            |> List.map (fun (x::y::z::[]) -> (x,y,z) |> matcher)
            |> List.indexed
            |> List.filter (fun (a,b) -> b <> None)
            |> List.tryHead

        if firstMatch = None 
        then None
        else 
            let idx = firstMatch.Value |>fst
            let v = (firstMatch.Value |>snd).Value
            let resultTkns = tkns[..idx-1]@[v]@tkns[idx+3..]
            Some (resultTkns,resultTkns)

    let lex2val (tkns:Lex list) =
        let evalPrior1 = evalWithMatcher matchPrior1
        let evalPrior2 = evalWithMatcher matchPrior2
        let evalPrior3 = evalWithMatcher matchPrior3

        let iterTkns (tknsEval0:Lex list) =
            if tknsEval0.Length = 1 then None else
            let tknsEval1 = tknsEval0 |> List.unfold evalPrior1 |> List.append [tknsEval0] |> List.last
            let tknsEval2= tknsEval1 |> List.unfold evalPrior2 |> List.append [tknsEval1]|> List.last
            let tknsEval3= tknsEval2 |> List.unfold evalPrior3 |> List.append [tknsEval2] |> List.last
            Some(tknsEval3,tknsEval3)

        let evald = tkns |> List.unfold iterTkns |> List.last
        evald
    
    let finalVal = lex2val lexN |> List.head

    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
