open System.IO

type Instruction = { Operation: string; Argument: int; Visited: bool }

let ParseInstruction (instruction: string) =
    let [| operation; argument |] = instruction.Split(" ")
    { Operation=operation; Argument=int argument; Visited=false }

let MakeMapWithIndexAsKey list =
    list
    |> List.mapi (fun i instruction -> (i, instruction))
    |> Map.ofList

let rec ExecuteUntilLoop i acc (allInstructions: Map<int, Instruction>) =
    let current = allInstructions |> Map.find i

    if current.Visited then
        acc
    else
        let instructionsWithCurrentVisited =
            allInstructions
            |> Map.change i (fun (Some instruction) -> Some { instruction with Visited = true })

        match current.Operation with
        | "nop" -> instructionsWithCurrentVisited |> ExecuteUntilLoop (i + 1) acc
        | "acc" -> instructionsWithCurrentVisited |> ExecuteUntilLoop (i + 1) (acc + current.Argument)
        | "jmp" -> instructionsWithCurrentVisited |> ExecuteUntilLoop (i + current.Argument) acc


let SolvePuzzle rawInstructions =
    let instructions =
        rawInstructions
        |> List.map ParseInstruction
        |> MakeMapWithIndexAsKey

    instructions |> (ExecuteUntilLoop 0 0)

type ExecuteUntilEndResult =
| RanToCompletion of int
| LoopDetected

let rec ExecuteUntilEnd i acc (wasSwapped: bool) (allInstructions: Map<int, Instruction>) =
    match allInstructions |> Map.tryFind i with
    | None -> RanToCompletion acc
    | Some current ->
        if current.Visited then
            LoopDetected
        else
            let instructionsWithCurrentVisited =
                allInstructions
                |> Map.change i (fun (Some instruction) -> Some { instruction with Visited = true })

            let executeNop wasSwapped = instructionsWithCurrentVisited |> ExecuteUntilEnd (i+1) acc wasSwapped
            let executeJmp wasSwapped = instructionsWithCurrentVisited |> ExecuteUntilEnd (i+current.Argument) acc wasSwapped

            match current.Operation with
            | "acc" -> (instructionsWithCurrentVisited |> ExecuteUntilEnd (i+1) (acc + current.Argument) wasSwapped)
            | "nop" ->
                match executeNop wasSwapped with
                | RanToCompletion result -> RanToCompletion result
                | LoopDetected ->
                    match wasSwapped with
                    | true -> LoopDetected
                    | false -> executeJmp true
            | "jmp" ->
                match executeJmp wasSwapped with
                | RanToCompletion result -> RanToCompletion result
                | LoopDetected ->
                    match wasSwapped with
                    | true -> LoopDetected
                    | false -> executeNop true

let SolvePuzzlePartTwo rawInstructions =
    let instructions =
        rawInstructions
        |> List.map ParseInstruction
        |> MakeMapWithIndexAsKey

    let (RanToCompletion result) = instructions |> (ExecuteUntilEnd 0 0 false)
    result

[<EntryPoint>]
let main argv =

    let lines =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer for part one is %d" (SolvePuzzle lines)
    printfn "Answer for part two is %d" (SolvePuzzlePartTwo lines)

    0
