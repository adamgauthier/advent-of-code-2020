open System.IO

type PuzzleInput = { Adapters: seq<int64>; Device:int64; Outlet:int64 }

let ParseInput input =
    let adapters = input |> Seq.map int64 |> Seq.sort
    let device = adapters |> Seq.max |> (+) 3L

    { Adapters=adapters; Device=device; Outlet=0L }

let GetJoltDifferences puzzleInput =
    let rec buildPathAndGetJoltDifferences adapters currentJolts device differences =
        match device - currentJolts with
        | differenceWithDevice when differenceWithDevice <= 3L ->
            List.append differences [differenceWithDevice]
        | _ ->
            let nextAdapter = adapters |> Seq.find (fun a ->
                let difference = (a - currentJolts)
                difference >= 1L && difference <= 3L
            )

            let newDifferences = List.append differences [(nextAdapter - currentJolts)]
            buildPathAndGetJoltDifferences adapters nextAdapter device newDifferences

    buildPathAndGetJoltDifferences puzzleInput.Adapters puzzleInput.Outlet puzzleInput.Device []

let SolvePuzzle input =
    let puzzleInput = ParseInput input

    let differences = GetJoltDifferences puzzleInput

    let oneJoltDifferenceCount = differences |> Seq.filter ((=) 1L) |> Seq.length
    let threeJoltDifferenceCount = differences |> Seq.filter ((=) 3L) |> Seq.length

    oneJoltDifferenceCount * threeJoltDifferenceCount


let CountAllArrangements puzzleInput =
    let mutable arrangementCountCache = Map.empty

    let rec countAllArrangements adapters currentJolts device =
        match arrangementCountCache |> Map.tryFind currentJolts with
        | Some cached -> cached
        | None ->
            match device - currentJolts with
            | differenceWithDevice when differenceWithDevice <= 3L -> 1L
            | _ ->
                let possibleAdapters = adapters |> Seq.filter (fun a ->
                    let difference = (a - currentJolts)
                    difference >= 1L && difference <= 3L
                )

                let arrangementCount =
                    possibleAdapters
                    |> Seq.sumBy (fun adapter -> countAllArrangements adapters adapter device)

                arrangementCountCache <-
                    arrangementCountCache |> Map.change currentJolts (fun _ -> Some arrangementCount)

                arrangementCount

    countAllArrangements puzzleInput.Adapters puzzleInput.Outlet puzzleInput.Device

let SolvePuzzlePartTwo input =
    let puzzleInput = ParseInput input

    CountAllArrangements puzzleInput


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
