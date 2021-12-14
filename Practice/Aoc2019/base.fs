module basefunc

open System
open Aoc21_Common
open FParsec
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.Security.Cryptography
open System.Text

type ENV = T | P

let loadFile (day:int) (env:ENV) =
    System.IO.Path.Combine ("d" + day.ToString("00") , (env |> function | T -> "test.txt" | P -> "input.txt"))