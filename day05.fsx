#load "input05.fsx"
#load "common.fsx"
open Input05
open Common

let rec resolve (min,max) chars = 
    match chars with
    | head :: tail -> 
        let diff = ((max-min)/2)
        if head = 'F' || head = 'L' then
            resolve (min, min+diff) tail
        else
            resolve (max-diff, max) tail
    | [] -> (min,max)

let resolveSeatId (line:string) =
    let chars = line.ToCharArray()|>Array.toList
    let (row,_) = List.take 7 chars |> resolve (0,127)
    let (col,_) = List.skip 7 chars |> resolve (0,7)
    row*8+col
    
resolveSeatId "BFFFBBFRRR" |> printfn "Example 1: %d"
resolveSeatId "FFFBBBFRRR" |> printfn "Example 2: %d"
resolveSeatId "BBFFBBFRLL" |> printfn "Example 3: %d"

let solveA input = 
    input |> splitStringInLines |> Seq.map resolveSeatId |> Seq.max

solveA inputA |> printfn "Result Input A: %d"

let solveB input =
    let seatIds = input |> splitStringInLines |> Array.map resolveSeatId |> Array.sort
    
    // find the first pair which has more than 1 id in between=> mine is (that one +1)
    seatIds |> Array.pairwise |> Array.find (fun (x,y) -> x+1<>y) |> fun (x,_) -> x+1

solveB inputA |> printfn "Result Input B: %d"