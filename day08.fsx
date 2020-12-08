#load "input08.fsx"
#load "common.fsx"
open Input08
open Common
open System

type Operation =
    | Nop of int
    | Acc of int
    | Jmp of int
type State = {nextInstruction:int; accumulator:int}
type Program = Operation[]
let programSize program = program |> Seq.length

let parse (input:string) =
    let inputParts = input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    match inputParts.[0] with
    | "nop" -> Nop (int inputParts.[1]) 
    | "acc" -> Acc (int inputParts.[1]) 
    | "jmp" -> Jmp (int inputParts.[1]) 
    | i -> failwith (sprintf "invalid instruction found: %s" i)

let parseProgram (s:string):Program = s |> splitStringInLines |> Array.map parse

let processInstr state instr =
    match instr with
    | Nop (_) -> {state with nextInstruction = state.nextInstruction + 1}
    | Acc (x) -> {state with nextInstruction = state.nextInstruction + 1; accumulator = state.accumulator + x}
    | Jmp (x) -> {state with nextInstruction = state.nextInstruction + x}
    
let solveA (s:string) =
    let instructions = parseProgram s
    let rec detectLoop (state:State) (visited:int list) = 
        let loopDetected = visited |> Seq.exists (fun x -> x=state.nextInstruction)
        if loopDetected then
            state.accumulator
        else
            let nextInstr = instructions.[state.nextInstruction]
            let nextState = processInstr state nextInstr
            detectLoop nextState (visited @ [state.nextInstruction])

    detectLoop {nextInstruction=0; accumulator=0} []
        
example |> solveA |> printfn "Result Example A: %d"
input |> solveA |> printfn "Result Input A: %d"

let rec generatePrograms (initialProgram:Program) (pos:int) (newPrograms:Program list) =
    if pos = (programSize initialProgram) then newPrograms 
    else 
        let cpy = initialProgram |> Array.copy
        let additionalPrograms = 
            match initialProgram.[pos] with
            | Nop (x) -> 
                        cpy.[pos] <- Jmp(x)
                        [cpy]
            | Jmp (x) -> 
                        cpy.[pos] <- Nop(x)
                        [cpy]
            | _ -> []
        generatePrograms initialProgram (pos+1) (newPrograms @ additionalPrograms)

let rec detectLoopOrResult (prog:Program) (state:State) (visited:int list) = 
    let loopDetected = visited |> Seq.exists (fun x -> x=state.nextInstruction)
    let outOfBootCode = state.nextInstruction >= (programSize prog)
    if loopDetected then
        None
    else if outOfBootCode then
        Some(state.accumulator)
    else
        let nextInstr = prog.[state.nextInstruction]
        let nextState = processInstr state nextInstr
        detectLoopOrResult prog nextState (visited @ [state.nextInstruction])

let solveB (s:string) =
    let program = parseProgram s
    let variants = generatePrograms program 0 [program]
    variants |> Seq.map(fun x-> detectLoopOrResult x {nextInstruction=0; accumulator=0} []) |> Seq.choose id |> Seq.head

example |> solveB |> printfn "Result Example B: %d"
input |> solveB |> printfn "Result Input B: %d"