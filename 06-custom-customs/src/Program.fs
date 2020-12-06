open System.IO

let CleanInput (input: string) =
    input.Replace("\r\n", "\n")

let SolvePuzzle (input: string) =
    let cleaned = CleanInput input

    let groups = cleaned.Split("\n\n")

    let answeredQuestionsPerGroup =
        groups
        |> Seq.map (fun groupLines -> groupLines.Replace("\n", "") |> Seq.distinct)

    let answeredQuestionsCountPerGroup =
        answeredQuestionsPerGroup
        |> Seq.map (fun answeredQuestions -> answeredQuestions |> Seq.length)

    Seq.sum answeredQuestionsCountPerGroup

[<EntryPoint>]
let main argv =

    let input =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> String.concat "\n"

    printfn "Answer for part one is %d" (SolvePuzzle input)

    0
