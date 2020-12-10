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


let FindRangeThatSumsTo target allNumbers =
    let rec findRange firstIndex currentIndex target allNumbers =
        if currentIndex >= List.length allNumbers then
            findRange (firstIndex + 1) 0 target allNumbers
        else
            let range = allNumbers.[firstIndex..currentIndex]

            match Seq.sum range with
            | sum when sum = target -> range
            | _ -> findRange firstIndex (currentIndex + 1) target allNumbers

    findRange 0 0 target allNumbers

let GetMinAndMax sequence =
    (Seq.min sequence, Seq.max sequence)

let SolvePuzzlePartTwo target rawNumbers =
    let numbersBeforeTarget =
        rawNumbers
        |> List.map int64
        |> List.takeWhile (fun n -> n <> target)

    numbersBeforeTarget
    |> FindRangeThatSumsTo target
    |> GetMinAndMax
    |> fun (min, max) -> min + max


[<EntryPoint>]
let main argv =

    let lines =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    let partOneAnswer = SolvePuzzle 25 lines

    printfn "Answer for part one is %d" partOneAnswer
    printfn "Answer for part two is %d" (SolvePuzzlePartTwo partOneAnswer lines)

    0
