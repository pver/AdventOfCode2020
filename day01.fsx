#load "input01.fsx"
open Input01

let pair arr = 
    let arrBigToSmall = Array.sortDescending arr
    let arrSmallToBig = Array.rev arrBigToSmall

    let rec search pos1 pos2 =
        let num1 = arrBigToSmall.[pos1]
        let num2 = arrSmallToBig.[pos2]
        let sum = num1 + num2

        if sum = 2020 
        then (num1,num2)
        else
            if sum > 2020 then search (pos1+1) 0
            else search pos1 (pos2+1)
    
    search 0 0

let solveA input = pair input |> fun (a,b) -> a*b

solveA exampleA |> printfn "Result Example A: %d"
solveA inputA |> printfn "Result Input A: %d"

let triple arr = 
    let arrSmallToBig = Array.sort arr
    let len = Array.length arr

    let rec search pos1 pos2 pos3 =
        let num1 = arrSmallToBig.[pos1]
        let num2 = arrSmallToBig.[pos2]
        let num3 = arrSmallToBig.[pos3]
        let sum = num1 + num2 + num3
        if sum = 2020 
        then (num1,num2,num3)
        else
            if sum < 2020 then
                let next3 = (pos3 + 1)
                let next2 = pos2 + (next3/len)
                let next1 = pos1 + (next2/len)
                search (next1%len) (next2%len) (next3%len)
            else if num1+num2 > 2020 then
                search (pos1+1) 0 0
            else 
                let next2 = pos2 + 1
                let next1 = pos1 + (next2/len)
                search (next1%len) (next2%len) 0
    
    search 0 0 0

let solveB input = triple input |> fun (a,b,c) -> a*b*c
solveB exampleA |> printfn "Result Example B: %d"
solveB inputA |> printf "Result Input B: %d"