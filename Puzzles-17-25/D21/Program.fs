open System
open Aoc21_Common
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
let internal (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
let internal (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)
// Z3 stuff
let internal (??>) (b:Bool) (expr1:Theory,expr2:Theory) = 
    match expr1,expr2 with
    | (:? Bool as b1),(:? Bool as b2) -> createITE (b |> asBoolExpr) (b1 |> asBoolExpr) (b2 |> asBoolExpr)
    | (:? Int as i1),(:? Int as i2) -> createITE (b |> asBoolExpr) (i1 |> asIntExpr) (i2 |> asIntExpr)
    | (:? Real as r1),(:? Real as r2) -> createITE (b |> asBoolExpr) (r1 |> asRealExpr) (r2 |> asRealExpr)
    | _ ->failwith "Failed to match types in ?>"
let internal (-->) (expr1) (expr2) = (expr1,expr2)

//type State = { Node:string; }

type DieState = { Die: int; TotalDieCount: int; }
type PlayerState = { Pos: int; Score: int; }
type GameState = { ActivePlayer: PlayerState; PassivePlayer: PlayerState; DS: DieState; }
type CountWins = { WinsForActive: int64; WinsForPassive: int64; }


[<EntryPoint>]
let main argv =    
    let env = P
    let inputFile = env |> function | T -> "test.txt" | P -> "input.txt"
    
    let lns = 
        C.readLines inputFile
        |> List.map (
            fun (ln:string) -> 
                ln
            )
    0

    //let mutable die = 1
    //let mutable totalDie = 0

    let incDieUntyped (r0,(die,totalDie)) =
        let r = die
        let totalDie = totalDie+1
        let die = die + 1
        let die = if die > 100 then 1 else die
        r0+r,(die,totalDie)

    let throwDieUntyped (die,totalDie:int) =
        let r,(die,totalDie) = (0,(die,totalDie)) |> incDieUntyped |> incDieUntyped |> incDieUntyped
        r,(die,totalDie)

    let p1 = if env=P then 2 else 4
    let p2 = if env=P then 10 else 8

    let p1s = (p1, 0)
    let p2s = (p2, 0)

    let playerStepUntyped (p,s) (die,totalDie:int) =
        let d, (die,totalDie) = throwDieUntyped (die,totalDie)
        let p2 = ((p+d) % 10) |> fun x -> if x =0 then 10 else x
        (p2,s+p2),(die,totalDie)

    let stepUntyped (((p1,s1),(p2,s2)),(die,totalDie:int)) =
        if s2 >= 1000 then None else
        if s1 >= 1000 then None else

        let (nextP1, nextS1),(die,totalDie:int) = playerStepUntyped (p1,s1) (die,totalDie)
        if nextS1 >= 1000 then 
            let r = ((nextP1, nextS1),(p2,s2)),(die,totalDie)
            Some (r,r)
        else
            let (nextP2, nextS2),(die,totalDie) = playerStepUntyped (p2,s2) (die,totalDie)
            let r = ((nextP1, nextS1),(nextP2,nextS2)),(die,totalDie)
            Some (r,r)

    let s0 = (p1s,p2s),(1,0)

    let ((f1,fs1),(f2,fs2)),(die,totalDie) = s0 |> Seq.unfold stepUntyped |> Seq.last

    let loser = min fs1 fs2
    let res1 = loser * totalDie



    let p1 = if env=P then { Pos=2; Score=0 } else { Pos=4; Score=0 }
    let p2 = if env=P then { Pos=10; Score=0 } else { Pos=8; Score=0 }

    // remake part1, this time typed
    
    //let incDie (r0 : int,die:DieState) =
    //    let r = die
    //    let die = die + 1
    //    let die = if die > 100 then 1 else die
    //    r0+r, { Die = die ; TotalDieCount = die.TotalDieCount+1 ;  }

    //let throwDiePart1 (die: DieState) : (int * DieState) =
    //    let r,die2 = (0,die) |> incDie |> incDie |> incDie
    //    r,die2
    
    let addCountWins cw1 cw2 = 
        { 
            WinsForActive = cw1.WinsForActive + cw2.WinsForActive;
            WinsForPassive = cw1.WinsForPassive + cw2.WinsForPassive; }
    
    let playerStep (ps:PlayerState) (x,y,z) =
        let p2 = ((ps.Pos+x+y+z) % 10) |> fun x -> if x =0 then 10 else x ;
        let ps2 = { Pos = p2; Score = ps.Score + p2; }
        ps2


    let cachedPos = Dictionary<GameState,CountWins>()

    let generateInnerStates (g:GameState) =
        List.allPairs [1..3] [1..3]
        |> List.allPairs [1..3]
        |> List.map (fun (x,(y,z)) -> (x,y,z))
        |> List.map (playerStep g.ActivePlayer) 
        |> List.map (fun psn -> { g with ActivePlayer = g.PassivePlayer; PassivePlayer = psn; })

    let rec calcWins (g:GameState) : CountWins = 
        let v = 
            if g.ActivePlayer.Score >= 21 then { WinsForActive=1;WinsForPassive=0; } else
            if g.PassivePlayer.Score >= 21 then { WinsForActive=0;WinsForPassive=1; } else
            
            let innerStates = generateInnerStates g
            let innerCalcWins = innerStates |> List.map getWins
            let outerCalcWin = innerCalcWins |> List.reduce addCountWins |> fun cw -> { WinsForActive = cw.WinsForPassive; WinsForPassive = cw.WinsForActive; }
            
            outerCalcWin
        cachedPos[g] <- v
        v

    and getWins (g:GameState) : CountWins =
        let found, value = cachedPos.TryGetValue g
        if found 
        then 
            value 
        else
            calcWins g

    let g0 = { ActivePlayer= p1; PassivePlayer= p2; DS = { Die=1; TotalDieCount=0; } }
    let finalCountWins = getWins g0
    let res2 = max finalCountWins.WinsForActive finalCountWins.WinsForPassive // player 1

    0

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code


