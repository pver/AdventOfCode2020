#load "input09.fsx"
#load "common.fsx"
open Input09
open Common

let parse (input:string) = input |> splitStringInLines |> Array.map int64

let findInvalidNumber (numbers:int64[]) (preambleSize:int) = 
    let rec hasSum posA posB toMatch =
        if posA = posB then false
        else
            let firstNumber = numbers.[posA]
            let secondNumber = toMatch - firstNumber
            let found = numbers |> Seq.skip posA |> Seq.take (posB-posA) |> Seq.exists (fun x-> x = secondNumber)
            if found then true
            else hasSum (posA+1) posB toMatch

    numbers |> Seq.skip preambleSize |> Seq.mapi (fun idx x -> if hasSum idx (idx+preambleSize) x then None else Some(x)) |> Seq.choose id |> Seq.head

let solveA (input:string) = parse input |> findInvalidNumber

solveA example examplePreamble |> printfn "Result Example A: %d"
solveA input inputPreamble |> printfn "Result Input A: %d"

let solveB (input:string) (preambleSize:int) =
    let numbers = parse input
    let invalidNumber = findInvalidNumber numbers preambleSize

    let rec findSum posA posB =
        let windowValues = numbers |> Seq.skip posA |> Seq.take (posB-posA)
        let sum = Seq.sum windowValues 
        
        if sum = invalidNumber then 
            let min = Seq.min windowValues
            let max = Seq.max windowValues
            Some((min, max))
        else if sum > invalidNumber then None
        else findSum posA (posB+1)

    let (min,max) = numbers |> Seq.mapi (fun idx x -> findSum idx idx) |> Seq.choose id |> Seq.head
    min+max

solveB example examplePreamble |> printfn "Result Example B: %d"
solveB input inputPreamble |> printfn "Result Input B: %d"