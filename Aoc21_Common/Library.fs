namespace Aoc21_Common

open System

module Array2D =
    let toListOfLists (arr2d: 'a [,]) = [ yield! [arr2d.GetLowerBound(0)..arr2d.GetUpperBound(0)] |> List.map (fun i -> arr2d.[i,*] |> List.ofArray) ]


module C =
    //let inline (+?) (x: int) (y: int) = x + 2*y

    //let abc = 1 +? 2

    let MOVES_PLUS = [(-1,0);(+1,0);(0,-1);(0,+1)]
    let MOVES_X = [(-1,-1);(-1,+1);(+1,-1);(+1,+1)]
    let MOVES_8DIR = [(-1,0);(+1,0);(0,-1);(0,+1);(-1,-1);(-1,+1);(+1,-1);(+1,+1)]

    let append (s: #seq<'T>) (toAppend: #seq<'T>) =
        Seq.append toAppend s

    let prepend (s: #seq<'T>) (toAppend: #seq<'T>) =
        Seq.append s toAppend
    
    let toDictionary (map : Map<_, _>) : System.Collections.Generic.Dictionary<_, _> = 
        System.Collections.Generic.Dictionary(map)

    let toPosMap (ln2elts : string -> #seq<'T>) (lns : string list) : Map<(int*int),'T> =
        lns
        |> List.indexed
        |> List.map (
            fun (y, ln:string) -> 
                ln |> ln2elts |> Seq.indexed |> Seq.map (fun (x,n) -> ((x,y),n)) |> List.ofSeq
            )
        |> List.concat
        |> Map.ofList

    let walled (pos2val: (int*int) -> 'T) (m : Map<(int*int),'T>) : Map<(int*int),'T>  =
        let (minx, maxx) = m.Keys |> Seq.map fst |> fun xs -> (Seq.min xs |> (+) -1, Seq.max xs |> (+)1)
        let (miny, maxy) = m.Keys |> Seq.map snd |> fun ys -> (Seq.min ys |> (+) -1, Seq.max ys |> (+)1)

        let m1 = 
            [minx..maxx] 
            |> List.fold (fun m x -> 
                let pos0 = (x, miny)
                let posn = (x, maxy)
                m
                |> Map.add pos0 (pos2val(pos0))
                |> Map.add posn (pos2val(posn))
            ) m
        let m2 = 
            [miny..maxy] 
            |> List.fold (fun m y -> 
                let pos0 = (minx, y)
                let posn = (maxx, y)
                m
                |> Map.add pos0 (pos2val(pos0))
                |> Map.add posn (pos2val(posn))
            ) m1
        m2

    let rec private skipLastEmpty (list:List<string>) =
        match list with
        | [] -> []
        | [""] -> []
        | [x] -> [x]
        | head :: tail -> head :: skipLastEmpty(tail)

    let tryParseInt (s:string) = 
        try 
            s |> int |> Some
        with :? FormatException -> 
            None
    

    let readLines fpath = 
        System.IO.File.ReadLines(fpath)
        |> List.ofSeq
        |> skipLastEmpty

    let splitStr (delimiter:string) (str: string) : string list =
        (
            if delimiter = " " 
            then str.Split() |> Array.filter (String.IsNullOrEmpty >> not)
            else str.Split(delimiter,StringSplitOptions.RemoveEmptyEntries)
        )
        |> List.ofArray
        
    let splitStrMulti (delimiters:string list) (str: string) : string list =
        str.Split(delimiters |> Array.ofList,StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        
    let splitCh (delimiter:string) (str: string) : string list =
        (
            if delimiter = " " 
            then str.Split() |> Array.filter (String.IsNullOrEmpty >> not)
            else str.Split(delimiter.ToCharArray(),StringSplitOptions.RemoveEmptyEntries)
        )
        |> List.ofArray


    //// from https://github.com/andreasjhkarlsson/aoc-2017/blob/master/Common.fs
    let parseList fn (str: string) =
        str.Split([|'\t'; ' '; '\r'; '\n'|])
        |> Array.choose (fun e ->
            match e.Trim() with
            | "" -> None
            | e -> Some (fn e))
    
    //// from https://github.com/andreasjhkarlsson/aoc-2017/blob/master/Common.fs
    let parseLines (str: string) =
        str.Split([|'\r'; '\n'|])
       |> Array.choose (fun row ->
            match row.Trim() with
            | "" -> None
            | row -> Some row)
    
    //// from https://github.com/andreasjhkarlsson/aoc-2017/blob/master/Common.fs
    let parseMatrix fn str =
       str
       |> parseLines
       |> Array.map (parseList fn)
       |> Array.filter (not << Array.isEmpty)

    let rec gcd x y =
        if y = 0 then x
        else gcd y (x % y)

    let lcm a b = a*b/(gcd a b)

    let isPrime a =
        match a with
        | a when a < 2 -> false
        | a ->
            let divisors = seq { 2 .. int (sqrt (float a)) }
            not <| Seq.exists (fun d -> a % d = 0) divisors

    let print2d (s : #seq<#seq<'T>>) (morphVal) =
        for y in s do
            Console.WriteLine()
            for x in y do
                let v = morphVal x
                Console.Write(v.ToString()) 

    let print2dMap (s:Map<(int*int),'T>) (morphVal:'T->'U) =
        s
        |> Map.toSeq
        |> Seq.groupBy (fst >> fst)
        |> Seq.map (
            fun (y,row) ->
                Console.WriteLine();
                row
                |> Seq.sortBy (fst >> snd)
                |> Seq.iter (fun (pos,elt) -> Console.Write(morphVal elt))
            )
        |> List.ofSeq
        |> ignore

    let print2dBool (s) (toBool: 'T->bool) =
        for y in s do
            Console.WriteLine()
            for x in y do
                let v = toBool x
                if v 
                then Console.Write("█") 
                else Console.Write(" ") 
                

    let print2dArr2Dbool (arr:'T[,]) (toBool: 'T->bool) =
        let h = arr.GetLength(0)
        let w = arr.GetLength(1)

        for y in [0..h-1] do
            Console.WriteLine()
            for x in [0..w-1] do
                let v = toBool arr[y,x]
                if v 
                then Console.Write("█") 
                else Console.Write(" ") 

    //let (+) (x0,y0) (x1,y1) = (x0+x1,y0+y1)

    let ModularInverse modulo num = 
        let m = modulo
        let a = num
        let rec eea t t' r r' =
            match r' with
            | 0 -> t
            | _ -> 
                let div = r/r'
                eea t' (t - div * t') r' (r - div * r')
        (m + eea 0 1 m a) % m
    
    let ChineseRemainderTheorem remainders modulos =
        let n = remainders
        let g = modulos
        match Seq.fold(fun n g->if (gcd n g)=1 then n*g else 0) 1 g with
        |0 -> None
        |fN-> Some ((Seq.fold2(fun n i g -> n+i*(fN/g)*(ModularInverse g ((fN/g)%g))) 0 n g)%fN)

    let rec generateCombinationsNonRepeating (eltList : 'T list) : 'T list list =
        match eltList with
        | [x] -> [[x]]
        | x -> 
            [0..x.Length-1]
            |> List.map (fun idx ->
                let elt = eltList[idx]
                let innerChList = eltList |> List.except [elt]
                let r = 
                    generateCombinationsNonRepeating innerChList
                    |> List.map (fun arr -> elt::arr)
                r
            )
            |> List.concat

    let generateCombinationsRepeating (eltList : 'T list) : 'T list list =
        let rec generateCombinationsRepeatingInner (eltsRemaining:int) : 'T list list =
            match eltsRemaining with
            | 1 -> eltList |> List.map (fun elt -> [elt])
            | _ -> 
                [0..eltList.Length-1]
                |> List.map (fun idx ->
                    let elt = eltList[idx]
                    let r = 
                        generateCombinationsRepeatingInner (eltsRemaining-1)
                        |> List.map (fun arr -> elt::arr)
                    r
                )
                |> List.concat
        generateCombinationsRepeatingInner (eltList.Length)

    type HexCoordCube = { Q: int; R: int; S: int; }

    let hexCoords_axial2cube (x,y) = { Q=x; R=y; S= -x-y; }
    let hexCoords_cube2axial (c:HexCoordCube)  = (c.Q, c.R)

    let hexCoords_GetNeighbours (x,y) =
        (x,y)
        |> hexCoords_axial2cube
        |> fun c -> [
            { c with Q=c.Q+1; R=c.R-1; };
            { c with Q=c.Q+1; S=c.S-1; };
            { c with R=c.R+1; Q=c.Q-1; };
            { c with R=c.R+1; S=c.S-1; };
            { c with S=c.S+1; Q=c.Q-1; };
            { c with S=c.S+1; R=c.R-1; };
            ]
        |> List.map hexCoords_cube2axial

    let hexCoords_Dist (x1,y1) (x2,y2) =
        let c1 = hexCoords_axial2cube (x1,y1)
        let c2 = hexCoords_axial2cube (x2,y2)

        seq { abs(c1.Q-c2.Q) ; abs(c1.R-c2.R) ; abs(c1.S-c2.S) }
        |> Seq.max

    let hexCoords_IsNeighbour (x1,y1) (x2,y2) =
        hexCoords_Dist (x1,y1) (x2,y2)
        |> (=)1

