#load "input02.fsx"
#load "common.fsx"
open Input02
open Common

type PwdEntry = { Min: int; Max: int; Ch: char; Pwd:string; } // 1-3 a: abcde

let parsePwdEntry (s:string) = 
    let splitted = s.Split([|'-';' ';':'|], System.StringSplitOptions.RemoveEmptyEntries)
    let pwd = splitted.[3]
    let min = int splitted.[0]
    let max = int splitted.[1]
    let ch = splitted.[2].[0]
    {Min=min; Max=max; Ch=ch; Pwd=pwd}

let isValidEntryA (entry:PwdEntry) = 
    let occurrences = entry.Pwd.ToCharArray() |> Seq.filter (fun c->c=entry.Ch) |> Seq.length 
    entry.Min <= occurrences && occurrences <= entry.Max

let isValidEntryB (entry:PwdEntry) = 
    let occurrences = entry.Pwd.ToCharArray()
    let c1 = occurrences.[entry.Min-1]
    let c2 = occurrences.[entry.Max-1]
    (c1 = entry.Ch || c2 = entry.Ch) && (c1<>c2)

let solve (fullInput:string) (validationEntry) =
    fullInput |> splitStringInLines |> Array.map parsePwdEntry |> Array.filter validationEntry |> Array.length

let solvePartA (fullInput:string) = solve fullInput isValidEntryA
let solvePartB (fullInput:string) = solve fullInput isValidEntryB
    
solvePartA exampleA |> printfn "Result Example A: %d"
solvePartA inputA |> printfn "Result Input A: %d"

solvePartB exampleA |> printfn "Result Example B: %d"
solvePartB inputA |> printfn "Result Input B: %d"
