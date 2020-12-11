open System.IO

let GetJoltDifferences (adapters: seq<int64>) outlet device =
    let rec buildPathAndGetJoltDifferences (adapters: seq<int64>) currentJolts device differences =
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

    buildPathAndGetJoltDifferences adapters outlet device []

let SolvePuzzle input =
    let adapters = input |> Seq.map int64 |> Seq.sort
    let outlet = 0L
    let device = (Seq.max adapters) + 3L

    let differences = GetJoltDifferences adapters outlet device

    let oneJoltdifferenceCount = differences |> Seq.filter ((=) 1L) |> Seq.length
    let threeJoltdifferenceCount = differences |> Seq.filter ((=) 3L) |> Seq.length

    oneJoltdifferenceCount * threeJoltdifferenceCount


[<EntryPoint>]
let main argv =

    let lines =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer for part one is %d" (SolvePuzzle lines)

    0
