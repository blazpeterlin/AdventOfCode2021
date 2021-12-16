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

type Operator = SUM | PRODUCT | MIN | MAX | GT | LT | EQ
type ValueDef = Literal of Int64 | Op of Operator
type Packet = { ver:Int64; typeId: Int64; valueDef: ValueDef; InnerPackets: Packet list; StrAfter: string list }

//type State = { str:string; }

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"

    // C.toPosMap (fun (str) -> str.ToCharArray())
    
    let fromHex (s:string) = 
      s.ToCharArray()
      |> Seq.map (fun ch -> 
            match ch with 
             | '0' -> "0000"
             | '1' -> "0001"
             | '2' -> "0010"
             | '3' -> "0011"
             | '4' -> "0100"
             | '5' -> "0101"
             | '6' -> "0110"
             | '7' -> "0111"
             | '8' -> "1000"
             | '9' -> "1001"
             | 'A' -> "1010"
             | 'B' -> "1011"
             | 'C' -> "1100"
             | 'D' -> "1101"
             | 'E' -> "1110"
             | 'F' -> "1111"
             | _ ->failwith "f"
      )
      |> List.ofSeq
      |> List.fold (+) ""
      |> (fun s ->s.ToCharArray())
      |> List.ofArray
      |> List.map (fun ch ->ch.ToString())


    let lns = 
        C.readLines inputFile
        |> List.head
        |> fromHex
    0

    let str2val str = Convert.ToInt64(str, 2);
    let strs2str strs = strs |> Seq.fold (+) ""

    let st0 = lns

    let rec parsePacket (*(cutLength:bool)*) (str:string list)  =
        if str = [] then None else
        if (str |> List.tryFind ((=)"1"))=None then None else
        let x1::x2::x3::xrest = str
        let xstr = "" + x1 + x2 + x3

        let version =xstr |> str2val

        let t1::t2::t3::trest = xrest
        let typeID = "" + t1 + t2 + t3 |> str2val

        let isLiteral = typeID = 4
        let r = 
            if isLiteral
            then
                let literalSeq = 
                    Seq.initInfinite id
                    |> Seq.map (fun idx -> idx*5+6)
                    |> Seq.map (fun idx -> 
                        if str.Length <= idx then (idx, "",true) else
                        let firstBit = str[idx]
                        let valStr = str[idx+1..idx+4] |> List.fold (+) ""
                        (idx, valStr, firstBit="0")
                    )

                let finalIdx = 
                    literalSeq
                    |> Seq.find (fun (_,_,firstBit) -> firstBit)
                    |> fun (idx,_,_) -> idx

                let v = 
                    literalSeq
                    |> Seq.takeWhile (fun (idx,_,_) -> idx <= finalIdx)
                    |> Seq.map (fun (idx,v,_) -> v)
                    |> strs2str
                    |> str2val

                let cutLength=false
                let finalIdxPadded = if cutLength then (finalIdx + 4 + 4) - ((finalIdx + 4 + 4)%4) else finalIdx + 5
                let finalStr = str |> List.skip finalIdxPadded
                let totalPacket = { ver=version; typeId = typeID; valueDef = Literal(v); InnerPackets = []; StrAfter = finalStr }

                Some (totalPacket, finalStr)
            else
                let op = 
                    match (int typeID) with
                    | 0 -> SUM
                    | 1 -> PRODUCT
                    | 2 ->  MIN
                    | 3 -> MAX
                    | 5 -> GT
                    | 6 -> LT
                    | 7 -> EQ
                    | _ -> failwith "huh"

                let z1::zrest = trest

                if z1 = "0" then 
                    let totalLenSubPackets = zrest[0..15-1] |> strs2str |> str2val
                    //let finalLenStr = str.Length - 6 - 16 - totalLenSubPackets
                    let innerStr = str |> List.skip (6+16) |> List.take (int totalLenSubPackets)
                    let finalStr = str |> List.skip (6+16) |> List.skip (int totalLenSubPackets)
                    let (theseSubPackets : Packet list) = 
                        innerStr 
                        |> Seq.unfold (parsePacket)
                        |> List.ofSeq


                    Some({ ver=version; typeId = typeID; valueDef = Op(op); InnerPackets = theseSubPackets; StrAfter = finalStr }, finalStr)
                else
                    let numSubPackets = zrest[0..11-1] |> strs2str |> str2val

                    let innerStr = str |> List.skip (6+12)

                    let innerPackets = 
                        innerStr
                        |> Seq.unfold parsePacket
                        |> Seq.take (int numSubPackets)
                        |> List.ofSeq

                    let finalStr = innerPackets |> List.last |> fun st -> st.StrAfter

                    Some({ ver=version; typeId = typeID; valueDef = Op(op); InnerPackets = innerPackets; StrAfter = finalStr }, finalStr)
                        

        r
        

    let r1 = 
        st0
        |> List.unfold (parsePacket)
        //|> List.sumBy (fun p -> p.ver)

    let rec sumVersions (pckt:Packet) =
        pckt.ver + (pckt.InnerPackets |> List.sumBy sumVersions)

    let res1 = r1 |> List.sumBy sumVersions 

    let rec calcVal (pckt: Packet) =
        let innerVals = pckt.InnerPackets |> List.map calcVal
        match pckt.valueDef with
        | Literal(num) -> num
        | Op(SUM) -> innerVals |> List.fold (+) 0L
        | Op(PRODUCT) -> innerVals |> List.fold (*) 1L
        | Op(MIN) -> innerVals |> List.min
        | Op(MAX) -> innerVals |> List.max
        | Op(GT) -> if innerVals[0] > innerVals[1] then 1L else 0L
        | Op(LT) -> if innerVals[0] < innerVals[1] then 1L else 0L
        | Op(EQ) -> if innerVals[0] = innerVals[1] then 1L else 0L

    // 78346664827 too low
    let res2 = r1 |> List.map calcVal |>List.head

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


