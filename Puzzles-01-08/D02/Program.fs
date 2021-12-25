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



[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable lns = 
        C.readLines inputFile
        |> List.map (fun ln -> ln |> C.splitCh " ")
        //|> Seq.head
        //|> List.map (fun x -> x |> C.splitCh "[]")
        //|> C.parseList id
        //|> C.parseMatrix id
    0

    let (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)

    let res1 =
        lns
        |> List.map (fun (dir::strNum::[]) -> 
                    let num = int strNum
                    match dir with
                    | "forward" -> (num,0)
                    | "down" -> (0,num)
                    | "up" -> (0, -num)
                    | _ -> failwith "unknown"
                    )
        |> List.reduce (+..)
        |> (fun (x,y) -> x*y)
        
    let st0 = (bigint 0,bigint 0,bigint 0)

    let res2transformation = fun (x,y,z) (dir::strNum::[]) -> 
        let num = bigint (int strNum)
        match dir with
        | "forward" -> (x+num,y+num*z,z)
        | "down" -> (x,y,z+num)
        | "up" -> (x,y,z-num)
        | _ -> failwith "unknown"

    let res2tpl =
        lns
        |> List.fold res2transformation st0

    let res2 = res2tpl |> (fun (x,y,z) -> (x)*(y))
        
    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
