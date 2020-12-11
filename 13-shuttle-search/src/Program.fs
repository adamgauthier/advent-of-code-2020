open System.IO

let ParseInput (lines: string list) =
    let earliest = lines.[0] |> int64
    let busIds = lines.[1].Split(",") |> Seq.filter ((<>) "x") |> Seq.map int64
    (earliest, busIds)

let rec FindEarliestDeparture currentTimestamp busIds =
    let departsNow busId = (currentTimestamp % busId) = 0L

    match busIds |> Seq.tryFind departsNow with
    | Some busId -> (busId, currentTimestamp)
    | None -> FindEarliestDeparture (currentTimestamp + 1L) busIds

let SolvePuzzle lines =
    let (earliest, busIds) = ParseInput lines

    let (busId, timestamp) = FindEarliestDeparture earliest busIds

    busId * (timestamp - earliest)


[<EntryPoint>]
let main argv =

    let lines =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer for part one is %d" (SolvePuzzle lines)

    0
