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

type ENV = T | P


//type State = { Node:string; }

[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"

    let lns = 
        C.readLines inputFile
    0

    
    let ln0 = lns |> List.head |> (fun x -> x.ToCharArray() |> List.ofArray |> List.map (fun ch -> match ch with | '#' -> 1 | '.' -> 0 | _ -> failwith "huh" ))
    let mp = ln0 
    let img0 = 
        lns |>List.skip 2 |> C.toPosMap (fun x -> x.ToCharArray() |> List.ofArray |> List.map (fun ch -> match ch with | '#' -> 1 | '.' -> 0 | _ -> failwith "huh" ))
        |> Map.toList
        |> List.map (fun ((x,y),v) -> (x,y),v)
        |> Map.ofList

    let offsets = [(-1,-1);(0,-1);(1,-1);(-1,0);(0,0);(1,0);(-1,1);(0,1);(1,1)]
    
    let wall img =  img |> C.walled (fun (x,y) -> 0)

    let step img iteration = 
        let imgW0 = img |> wall
        let imgW = imgW0 |> wall
        //let imgW2 = imgW |> wall
        let coords = imgW0.Keys |> List.ofSeq

        let nextImg = 
            coords
            //|> List.filter (fun (x,y) -> (x,y)=(-2, -2))
            //|> List.filter (fun (x,y) -> )
            |> List.map (fun(x,y) -> 
                //let dbg = if (x,y)=(0,1) then 1 else 0

                //let isBoundary = not (imgW0.ContainsKey (x-1,y-1)) || not(imgW0.ContainsKey(x+1,y+1))
                let isBoundary2 = not (imgW.ContainsKey (x-1,y-1)) || not(imgW.ContainsKey(x+1,y+1))
                //let isBoundary2 = not (imgW2.ContainsKey (x-1,y-1)) || not(imgW2.ContainsKey(x+1,y+1))


                if isBoundary2
                then 
                    None// Some((x,y),mp[0])
                else
                    let rect = offsets |> List.map (fun offs -> (x,y) +.. offs)
                    //let idx = rect |> List.map (fun r -> imgW[r].ToString()) |> List.reduce (+) |> fun str ->  Convert.ToInt32(str, 2)
                    let idx = rect |> List.fold (fun x pos -> x*2+ imgW[pos]) 0
                    let v = mp[idx]
                    Some ((x,y),v)
                )
            |> List.choose id
            |> Map.ofList

        nextImg
        

    let calcPart1 img00 = 
        let img01a = [1..2+1] |> List.fold (fun img _ -> wall img) img00
        let img01b = [1..2] |> List.fold step (*img01a*) img01a
    
        //C.print2dMap img0 (fun x -> x.ToString())
        //C.print2dMap img1 (fun x -> x.ToString())
        //C.print2dMap img2 (fun x -> x.ToString())
    
        // not 5504
        // 5395
        let res1 = img01b |> Map.toSeq |> Seq.filter(fun ((x,y),v) -> img01a.ContainsKey (x,y)) |> Seq.map snd |> Seq.filter((=)1) |> Seq.length
        res1

    //let calcRes img =
    //    img |> Map.toSeq |> Seq.filter(fun ((x,y),v) -> img01a.ContainsKey (x,y)) |> Seq.map snd |> Seq.filter((=)1) |> Seq.length

    let res1 = calcPart1 img0

    //let res2 = [1..25] |> List.fold (fun (img:Map<int*int,int>) it -> calcPart1 img) img0
    
    let img02aFinalImgSized = [1..50] |> List.fold (fun img _ -> wall img) img0
    let img02a = [1..50+1] |> List.fold (fun img _ -> wall img) img02aFinalImgSized
    //let img02a = [1..50+1] |> List.fold (fun img _ -> wall img) img0
    let img02b = [1..50] |> List.fold step img02a
    // not 36787
    // not 20170 too high
    // not 19988 too high
    // 17584
    let res2 = img02b |> Map.toSeq |> Seq.filter(fun ((x,y),v) -> img02aFinalImgSized.ContainsKey (x,y)) |> Seq.map snd |> Seq.filter((=)1) |> Seq.length



    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


