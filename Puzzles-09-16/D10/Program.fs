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

//type BLOCK_DUMB = START | SQRL | SQRR | CRLL | CRLR | PRNL | PRNR | SGNL | SGNR | ERR of BLOCK_DUMB

type BRACES = SQUARE | CURLY | ROUND | ANGLED
type BLOCK = START | LEFT of BRACES | RIGHT of BRACES | ERR of BLOCK

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let lns = 
        C.readLines inputFile
        |> List.map (fun (ln:string) -> 
                ln.ToCharArray() 
                |> Array.map (function 
                    | '[' -> LEFT(SQUARE) | ']' -> RIGHT(SQUARE) 
                    | '(' -> LEFT(ROUND) | ')' -> RIGHT(ROUND) 
                    | '{' -> LEFT(CURLY) | '}' -> RIGHT(CURLY) 
                    | '<' -> LEFT(ANGLED) | '>' -> RIGHT(ANGLED) | _ -> failwith "unknown")
                |> List.ofArray
        )

    let unwrapStep (ln : BLOCK list, st : BLOCK list) =
        let st0::stt = st
        if (st0 |> function | ERR(_) -> true | _ -> false) then None else

        match ln with
        | [] -> None
        | ln0::lnt -> 
            let stNext = 
                match st0, ln0 with
                | LEFT(brace1), RIGHT(brace2) when brace1=brace2 -> stt
                | LEFT(brace1), RIGHT(brace2) when brace1<>brace2 -> ERR(ln0) :: st0 :: stt
                | _ -> ln0 :: st0 :: stt

            let r = (lnt, stNext)

            Some(r,r)

    let unwrap (ln : BLOCK list) =
        let st0 : BLOCK list = [START]
        let r = (ln, st0) |> List.unfold unwrapStep |> List.last |> snd
        r
        

    let res1 = 
        lns
        |> List.map unwrap
        |> List.filter (fun (st0::_stt) -> 
            match st0 with
            | ERR(_) -> true
            | _ -> false
            )
        |> List.map (fun (st0::_stt) -> 
            match st0 with
            | ERR(RIGHT(ROUND)) -> 3
            | ERR(RIGHT(SQUARE)) -> 57
            | ERR(RIGHT(CURLY)) -> 1197
            | ERR(RIGHT(ANGLED)) -> 25137
            | _ -> failwith "huh"
            )
        |> List.sum

    let res2 =
        lns
        |> List.map unwrap
        |> List.filter (fun (st0::_stt) -> 
            match st0 with
            | ERR(_) -> false
            | _ -> true
            )
        //|> List.concat
        |> List.map (
            fun blcks ->
                let r = 
                    blcks
                    |> List.filter (fun x -> x<>START)
                    |> List.map (function 
                        | LEFT(ROUND) -> 1L 
                        | LEFT(SQUARE) -> 2L 
                        | LEFT(CURLY) -> 3L 
                        | LEFT(ANGLED) -> 4L 
                        | _ ->failwith "huh")
                    |> List.fold (fun x y -> x*5L+y) 0L
                r
            )
        |> List.sort
        |> (fun scores -> scores[scores.Length/2])

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


