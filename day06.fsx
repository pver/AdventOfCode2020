#load "input06.fsx"
#load "common.fsx"
open Input06
open Common

let solveA input =
    let lines = (input + System.Environment.NewLine) |> splitStringInLinesKeepEmpty

    let (resultsByGroup,_) = 
        lines 
        |> Array.fold (fun (questions, (group:string)) x->
            if x = "" then 
                let distinctYesses = group.ToCharArray() |> Array.distinct |> Array.length
                (questions @ [distinctYesses], "")
            else
                (questions, (group+x))
        ) ([],"")
    resultsByGroup |> Seq.sum

solveA exampleA |> printfn "Result Input A: %d"
solveA inputA |> printfn "Result Input A: %d"


let solveB input =
    let lines = (input + System.Environment.NewLine) |> splitStringInLinesKeepEmpty

    let (resultsByGroup,_) = 
        lines 
        |> Array.fold (fun (questions, (group: Option<Set<char>> )) x->
            if x = "" then 
                let commonYesses = 
                    match group with
                    | None -> 0
                    | Some(g) -> g |> Seq.length
                (questions @ [commonYesses], None)
            else
                let groupSoFar = 
                    match group with
                    | None -> x.ToCharArray() |> Set.ofArray
                    | Some(g) -> Set.intersect g (x.ToCharArray() |> Set.ofArray)
                (questions, Some(groupSoFar))
        ) ([],None)

    resultsByGroup |> Seq.sum

solveB exampleA |> printfn "Result Input B: %d"
solveB inputA |> printfn "Result Input B: %d"
