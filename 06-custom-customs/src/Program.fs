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

let SolvePuzzlePartTwo (input: string) =
    let cleaned = CleanInput input

    let groups = cleaned.Split("\n\n")

    let answerPerGroup =
        groups
        |> Seq.map (fun groupLines -> groupLines.Split("\n"))

    let questionsEveryoneAnsweredCountPerGroup =
        answerPerGroup
        |> Seq.map (fun answers ->
            let allQuestions = answers |> String.concat "" |> Seq.distinct

            let hasEveryoneAnswered question =
                answers
                |> Seq.forall (fun entry -> entry |> (Seq.contains question))

            allQuestions
            |> Seq.filter hasEveryoneAnswered
            |> Seq.length
        )

    Seq.sum questionsEveryoneAnsweredCountPerGroup

[<EntryPoint>]
let main argv =

    let input =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> String.concat "\n"

    printfn "Answer for part one is %d" (SolvePuzzle input)
    printfn "Answer for part two is %d" (SolvePuzzlePartTwo input)

    0
