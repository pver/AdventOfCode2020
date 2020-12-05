#load "input04.fsx"
#load "common.fsx"
open Input04
open Common

let hasRequiredFields (lines:string []) =
    let fields = lines |> Array.map splitOnSpaces |> Array.concat
    let isValid = match Array.length fields with
        | 8 -> true
        | 7 -> fields |> (Array.exists (fun x->x.StartsWith("cid:"))) |> not
        | _ -> false
    isValid

let hasRequiredAndValidFields (lines: string[]) =
    if (not (hasRequiredFields lines)) then false
    else
        let fields = lines |> Array.map splitOnSpaces |> Array.concat
        let validField (f:string) = 
            match f.Split([|':'|]) with
            | [|fieldkey;fieldvalue|] -> 
                match fieldkey with
                | "byr" -> (isInt fieldvalue) && (1920 <= (int fieldvalue)) && ((int fieldvalue) <= 2002) // (Birth Year) - four digits; at least 1920 and at most 2002.
                | "iyr" -> (isInt fieldvalue) && (2010 <= (int fieldvalue)) && ((int fieldvalue) <= 2020) // (Issue Year) - four digits; at least 2010 and at most 2020.
                | "eyr" -> (isInt fieldvalue) && (2020 <= (int fieldvalue)) && ((int fieldvalue) <= 2030) // (Expiration Year) - four digits; at least 2020 and at most 2030.
                | "hgt" -> 
                            if fieldvalue.Length < 4 then false
                            else
                                let height = fieldvalue.Substring(0, fieldvalue.Length-2)
                                let unitt = fieldvalue.Substring(fieldvalue.Length-2)
                                if unitt = "cm" then (isInt height) && (150<=(int height) && (int height)<=193)
                                else if unitt = "in" then (isInt height) && (59<=(int height) && (int height)<=76)
                                else false // (Height) - a number followed by either cm or in: If cm, the number must be at least 150 and at most 193. If in, the number must be at least 59 and at most 76.
                | "hcl" -> fieldvalue.StartsWith("#") && (fieldvalue.Substring(1).ToCharArray()|>Array.forall (fun x -> (isDigit x) || ('a'<=x && x<='f'))) // (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
                | "ecl" -> ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> Seq.contains fieldvalue
                | "pid" -> (hasLength 9 fieldvalue) && (isLong fieldvalue) // (Passport ID) - a nine-digit number, including leading zeroes.
                | "cid" -> true
                | _ -> false
            | _ -> false
        fields |> Array.forall validField


let solve (input:string) (validation) =
    let lines = input |> splitStringInLinesKeepEmpty 
    
    let splitPositions = lines |> Array.mapi (fun i x -> if x = "" then Some(i) else None) |> Array.choose id
    let posWitStartEnd = Array.concat [[|0|]; splitPositions; [|(Array.length lines)|]] |> Array.pairwise
    let passwords = posWitStartEnd |> Array.map (fun (start,stop) -> Array.sub lines start (stop-start) |> Array.filter (fun x->x<>""))
    let validPasswords = passwords |> Array.filter (fun x -> validation x)
    Array.length validPasswords
    
let solveA (input:string) = solve input hasRequiredFields
let solveB (input:string) = solve input hasRequiredAndValidFields

solveA exampleA |> printfn "Result Example A: %d"
solveA inputA |> printfn "Result Input A: %d"

solveB exampleA |> printfn "Result Example B: %d"
solveB inputA |> printfn "Result Input B: %d"