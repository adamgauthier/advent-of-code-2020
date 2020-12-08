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

[<EntryPoint>]
let main argv =

    let lines =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer for part one is %d" (SolvePuzzle lines)

    0
