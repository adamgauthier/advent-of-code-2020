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


let rec GreatestCommonDenominator x y = if y = 0L then abs x else GreatestCommonDenominator y (x % y)

let LeastCommonMultiple x y = x * y / (GreatestCommonDenominator x y)

let FindEarliestTimestampSuccessiveDepartures step busIds =
    let rec findEarliestTimestampSuccessiveDepartures currentTimestamp (busIds: (int64 * int64) array) =
        let timestampToTest = (currentTimestamp - (snd busIds.[0]))
        let allMatches =
            busIds
            |> Array.forall (fun (i, busId) -> (timestampToTest + i) % busId = 0L)

        if allMatches then
            timestampToTest
        else
            findEarliestTimestampSuccessiveDepartures (currentTimestamp + step) busIds

    findEarliestTimestampSuccessiveDepartures 0L busIds

let SolvePuzzlePartTwo (lines: string list) =
    let busIds =
        lines.[1].Split(",")
        |> Array.indexed
        |> Array.filter (snd >> (<>) "x")
        |> Array.map (fun (i, id) -> (int64 i, int64 id))

    let busIdWithOffsetOfFirstBusId = busIds |> Array.find (fst >> (=) (snd busIds.[0]))

    let busIdsWithPerfectOffsetToTarget = busIds |> Array.filter (fun (i, id) ->
        let offsetToTarget = i - (fst busIdWithOffsetOfFirstBusId)
        abs offsetToTarget = id
    )

    let leastCommonMultiple =
        Array.append [| busIdWithOffsetOfFirstBusId |] busIdsWithPerfectOffsetToTarget
        |> Array.map snd
        |> Array.reduce LeastCommonMultiple

    FindEarliestTimestampSuccessiveDepartures leastCommonMultiple busIds


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
