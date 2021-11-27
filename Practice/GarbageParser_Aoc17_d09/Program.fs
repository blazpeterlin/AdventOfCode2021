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

type Inside = Normal | Garbage
type State = { Ins:Inside; Idx:int; Level: int; Score:int; CanceledGarbage:int; }

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let mutable ln = 
        C.readLines inputFile
        |> Seq.head
        //|> Seq.head
        //|> List.map (fun x -> x |> C.splitCh "[]")
        //|> C.parseList id
        //|> C.parseMatrix id
    0

    let st0 = { Ins=Normal; Idx=0; Level=0;  Score=0; CanceledGarbage=0; }
    
    let iterate (st:State) =
        if st.Idx >= ln.Length 
        then 
            None 
        else
            let nextSt =
                match st.Ins,ln[st.Idx] with
                | _,        '!'  -> { st with Idx=st.Idx+1; }
                | Garbage,  '>'  -> { st with Ins=Normal; }
                | Garbage,  _    -> { st with CanceledGarbage=st.CanceledGarbage+1; }
                | Normal,   '<'  -> { st with Ins=Garbage; }
                | Normal,   '{'  ->
                    let nextLvl = st.Level+1; 
                    { st with Level=nextLvl; Score=st.Score+nextLvl; }
                | Normal,   '}'  -> { st with Level=st.Level-1; }
                | Normal,   ','  -> st
                | Normal,   _    -> failwith "Unexpected"

            let nextSt2 = {nextSt with Idx=nextSt.Idx + 1}

            Some(nextSt2,nextSt2)

    let res1 = 
        st0
        |> Seq.unfold iterate
        |> Seq.last
        |> (fun x -> x.Score)

    let res2 = 
        st0
        |> Seq.unfold iterate
        |> Seq.last
        |> (fun x -> x.CanceledGarbage)

    0
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
