open System.IO
open System

type Instruction =
    | SetMask of list<char>
    | SetMem of int64 * int64

let ParseInstructions (lines: string list) =
    lines
    |> List.map (fun line ->
        if line.StartsWith("ma") then
            let mask = line.Split(" = ").[1]
            let maskChars = mask |> Seq.toList
            SetMask maskChars
        else
            let split = line.Split("] = ")
            let address = split.[0].Split("[").[1] |> int64
            let value = int64 split.[1]

            SetMem (address, value)
    )

let ExecuteInstructions instructions =
    let rec executeInstructions instructions i mask memory =
        match instructions |> List.tryItem i with
        | Some instruction ->
            let executeNextInstruction = executeInstructions instructions (i+1)

            match instruction with
            | SetMask value -> executeNextInstruction value memory
            | SetMem (address, value) ->
                let binaryValue = Convert.ToString(value, 2).PadLeft(36, '0')

                let maskedBinaryValue =
                    binaryValue
                    |> Seq.indexed
                    |> Seq.map (fun (i, originalChar) ->
                        match mask |> List.tryItem i with
                        | Some maskChar when maskChar <> 'X' -> maskChar
                        | _ -> originalChar
                    )
                    |> Seq.toArray
                    |> String

                let newValue = Convert.ToInt64(maskedBinaryValue, 2)

                let newMemory = memory |> Map.change address (fun _ -> Some newValue)

                executeNextInstruction mask newMemory
        | None -> memory

    executeInstructions instructions 0 [] Map.empty


let SolvePuzzle lines =
    let instructions = ParseInstructions lines

    let memory = ExecuteInstructions instructions

    memory
    |> Map.toSeq
    |> Seq.sumBy (fun (_, value) -> value)


[<EntryPoint>]
let main argv =

    let lines =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer for part one is %d" (SolvePuzzle lines)

    0
