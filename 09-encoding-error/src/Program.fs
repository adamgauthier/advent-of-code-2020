open System.IO

let rec GetAllPairs list =
    seq {
        match list with
        | head :: tail ->
            for element in tail do
                yield [head; element]
            yield! GetAllPairs tail
        | _ -> ()
    }

let IsNotTheSumOfAPreviousPair preambleSize (allNumbers: int64 list) (index, number) =
    let previousPairs = allNumbers.[index-preambleSize..index-1] |> GetAllPairs

    previousPairs
    |> (not << Seq.exists (fun ([first; second]) -> first + second = number))

let SolvePuzzle preambleSize rawNumbers =
    let numbers = rawNumbers |> List.map int64

    numbers
    |> List.indexed
    |> List.skip preambleSize
    |> List.find (IsNotTheSumOfAPreviousPair preambleSize numbers)
    |> snd


[<EntryPoint>]
let main argv =

    let lines =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer for part one is %d" (SolvePuzzle 25 lines)

    0
