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
open Microsoft.Z3.BitVec
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
    
    let lns = 
        C.readLines inputFile
        |> List.map (
            fun (ln:string) -> 
                ln |> C.splitCh "|" |> fun xs -> (xs[0] |> C.splitCh " ", xs[1] |> C.splitCh " ")
            )
    0

    let resetCtx () = Gs.overrideContext(Dictionary<string,string>())

    let ctx = resetCtx()

    let zsig = BitVec 7
    let zval = BitVecVal 7
    
    let names = "abcdefg"
    
    let chars = [0..6] |> List.map (fun i -> (1 <<< i) |> fun v -> (names[i],zval v)) |> Map.ofList
    let digits = [0..9] |> List.map (fun i -> i,zsig ("dig_"+i.ToString())) |> Map.ofList
    let realCharArr = chars.Values |> Array.ofSeq |> ArrayVal1D
    
    let opt = Opt()
        
    let mkRule dgt (str:string)  =
        let bvSumSegments = str.ToCharArray() |> List.ofArray |> List.map (fun ch -> chars[ch]) |> List.reduce (+)
        let bvDigit = digits[dgt]
        bvSumSegments =. bvDigit

    opt.Add(mkRule 0 "abcefg")
    opt.Add(mkRule 1 "cf")
    opt.Add(mkRule 2 "acdeg")
    opt.Add(mkRule 3 "acdfg")
    opt.Add(mkRule 4 "bcdf")
    opt.Add(mkRule 5 "abdfg")
    opt.Add(mkRule 6 "abdefg")
    opt.Add(mkRule 7 "acf")
    opt.Add(mkRule 8 "abcdefg")
    opt.Add(mkRule 9 "abcdfg")

    let iteratedResults = [
        for idx,(segments, outputVals) in (lns |> List.indexed) do
            opt.Push()

            let iterChars = names.ToCharArray() |> List.ofArray |> List.map (fun ch -> ch, zsig (ch.ToString() + "_" + idx.ToString())) |> Map.ofList
            let iterCharArr = iterChars.Values |> Seq.toArray |> ArrayVal1D

            opt.Add (iterCharArr |> Array1D.DISTINCT)
            let rulesCharBijection = [|
                    for ic in iterCharArr.Expr do
                        realCharArr.Expr |> Array.map (fun rc -> ic =. rc) |> ArrayVal1D |> Array1D.OR                    
                |]
            opt.Add (rulesCharBijection |> ArrayVal1D |> Array1D.AND)

            for seg in segments do
                let sumSeg = seg.ToCharArray() |> Array.map (fun ch -> iterChars[ch]) |> Array.reduce (fun ch1 ch2 -> ch1 + ch2)
                let ruleOneOfDigits = digits.Values |> Seq.map (fun dgt -> sumSeg =. dgt) |> Array.ofSeq |> ArrayVal1D |> Array1D.OR
                opt.Add ruleOneOfDigits

            opt.CheckOrFail()

            let intEval bv = opt.Eval bv |> fun x -> x.ToString() |> int

            let digitByTotal = 
                seq {
                    for num, digitBV in digits |> Map.toSeq do
                        let total = intEval digitBV
                        yield total, num
                }
                |> Map.ofSeq

            let outputNum = 
                seq {
                    for oval in outputVals do
                        let digitTotal = 
                            oval.ToCharArray() 
                            |> Array.map (fun ch -> iterChars[ch]) 
                            |> ArrayVal1D |> Array1D.SUM
                            |> intEval
                        let digit = digitByTotal[digitTotal]
                        digit
                }
                |> Seq.fold (fun x y -> x*10+y) 0

            opt.Pop()

            yield outputNum
        ]


    let res1 = 
        iteratedResults 
        |> List.map string 
        |> List.map (fun str -> str.ToCharArray() |> List.ofArray |> List.map string |> List.map int)
        |> List.concat
        |> List.filter (fun x -> [1;4;7;8] |> List.contains x)
        |> List.length

    let res2 = iteratedResults |> List.sum


    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


