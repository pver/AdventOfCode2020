#load "input07.fsx"
#load "common.fsx"
open Input07
open Common

type BagContent = {Count:int; Key:string}
type FromTo = {From:string; Too:string}

let parse (s:string) =
    let key = s.Split([|"bags"|],System.StringSplitOptions.RemoveEmptyEntries).[0].Trim()

    let contents =
        if s.Contains("no other bag") then [||]
        else
            s.Split([|"contain"|],System.StringSplitOptions.RemoveEmptyEntries).[1].Trim().Split([|','|])
            |>Array.map (fun x->x.Replace("bags","").Replace("bag","").Replace(".","").Trim())
            |>Array.map (fun x-> 
                let split = x.IndexOf(' ')
                {Count=(int (x.Substring(0, split))); Key=(x.Substring(split+1))}
            )
    (key, contents)

let parseAll (s:string) =
    s |> splitStringInLines |> Array.map parse

let rec resolveA (ensured:string []) (toInvestigate:string []) (listing:FromTo[]) =
    let newToInvestigate = listing |> Array.filter (fun x -> Array.contains x.Too toInvestigate) |> Array.map(fun x->x.From) |> Array.distinct
    let newEnsured = Array.concat [ensured; toInvestigate] |> Array.distinct

    if Array.isEmpty newToInvestigate 
    then (Array.length newEnsured) - 1 // -1 because discard shiny gold itself
    else resolveA newEnsured newToInvestigate listing

let solveA listing =
    let fromTos = listing |> Array.collect (fun (key, contents) -> Array.map (fun x-> {From=key; Too=x.Key}) contents)
    resolveA [||] [|"shiny gold"|] fromTos

solveA (parseAll exampleA) |> printfn "Result Example A: %d"
solveA (parseAll inputA) |> printfn "Result Input A: %d"


let rec resolveB (resolved:(string*int )[]) (listing:(string * BagContent []) []) =
    if Array.exists (fun (y,_) -> "shiny gold"=y) resolved then
        (Array.find (fun (y,_) -> "shiny gold"=y) resolved) |> fun (_,z) -> z - 1 // -1 for bag itself
    else
        let newResolved = 
            listing |> Array.map (fun (key,occ) -> 
                            if occ = [||] then Some((key,1)) 
                            else 
                                let allResolved = occ |> Array.forall (fun x-> Array.exists (fun (y,_) -> x.Key=y) resolved)
                                if allResolved then 
                                    let contentCount = 
                                        occ 
                                        |> Array.map (fun x-> (Array.find (fun (y,_) -> x.Key=y) resolved) |> fun (_,z) -> z*x.Count)
                                        |> Array.sum
                                    Some((key,contentCount + 1)) // +1 for bag itself
                                else None)
                    |> Array.choose id
        resolveB (Array.concat [resolved; newResolved]) listing
    
(parseAll exampleA) |> resolveB [||] |> printfn "Result Example B: %d"
(parseAll inputA) |> resolveB [||] |> printfn "Result Input B: %d"