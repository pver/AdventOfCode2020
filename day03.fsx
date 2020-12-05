#load "input03.fsx"
#load "common.fsx"
open Input03
open Common

let solve (inputMap:string) getNextPos =
    let lines = inputMap |> splitStringInLines
    let height = lines |> Array.length
    let width = lines.[0].Length

    let rec walk (row,col) (opencount, treecount) =
        if row >= height then (opencount, treecount)
        else
            let newCount = if lines.[row].[col%width] = '#' then (opencount, treecount+1) else (opencount+1, treecount)
            walk (getNextPos (row,col)) newCount
            
    let (_, treecount) = walk (0,0) (0,0)
    treecount
    
let solveA inputMap = solve inputMap (fun (row,col) -> (row+1,col+3))
solveA exampleA |> printfn "Result Example A: %d"
solveA inputA |> printfn "Result Input A: %d"

let solveB inputMap = 
    [
        (fun (row,col) -> (row+1,col+1));
        (fun (row,col) -> (row+1,col+3));
        (fun (row,col) -> (row+1,col+5));
        (fun (row,col) -> (row+1,col+7));
        (fun (row,col) -> (row+2,col+1));
    ] 
    |> Seq.map (fun x -> (int64) (solve inputMap x))
    |> Seq.fold (fun x treecount -> x*treecount) 1L

solveB exampleA |> printfn "Result Example B: %d"
solveB inputA |> printfn "Result Input B: %d"