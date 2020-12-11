open System.IO

let ParseInstruction (line: string) =
    let direction = line.Substring(0, 1)
    let value = line.Substring(1, line.Length - 1)
    (direction, int64 value)

let RunAllInstructions (lines: seq<string * int64>) =
    let rec runInstructions lines i currentDirection (eastWest, northSouth) =
        match lines |> Seq.tryItem i with
        | None -> (eastWest, northSouth)
        | Some (action, value) ->
            let runNextInstructions = runInstructions lines (i+1)

            match action with
            | "N" -> runNextInstructions currentDirection (eastWest, northSouth + value)
            | "S" -> runNextInstructions currentDirection (eastWest, northSouth - value)
            | "E" -> runNextInstructions currentDirection (eastWest + value, northSouth)
            | "W" -> runNextInstructions currentDirection (eastWest - value, northSouth)
            | "R" -> runNextInstructions ((currentDirection + value) % 360L) (eastWest, northSouth)
            | "L" ->
                let newDirection = currentDirection - value
                let newDirectionPositive =
                    if newDirection < 0L then
                        newDirection + 360L
                    else
                        newDirection
                runNextInstructions newDirectionPositive (eastWest, northSouth)
            | "F" ->
                match currentDirection with
                | 0L -> runNextInstructions currentDirection (eastWest, northSouth + value)
                | 90L -> runNextInstructions currentDirection (eastWest + value, northSouth)
                | 180L -> runNextInstructions currentDirection (eastWest, northSouth - value)
                | 270L -> runNextInstructions currentDirection (eastWest - value, northSouth)

    runInstructions lines 0 90L (0L, 0L)


let RunAllInstructionsWithWaypoint (lines: seq<string * int64>) =
    let rec runInstructions lines i (eastWest, northSouth) (waypointEast, waypointNorth) =
        match lines |> Seq.tryItem i with
        | None -> (eastWest, northSouth)
        | Some (action, value) ->
            let runNextInstructions = runInstructions lines (i+1)

            match action with
            | "N" -> runNextInstructions (eastWest, northSouth) (waypointEast, waypointNorth + value)
            | "S" -> runNextInstructions (eastWest, northSouth) (waypointEast, waypointNorth - value)
            | "E" -> runNextInstructions (eastWest, northSouth) (waypointEast + value, waypointNorth)
            | "W" -> runNextInstructions (eastWest, northSouth) (waypointEast - value, waypointNorth)
            | "L" ->
                match value with
                | 90L -> runNextInstructions (eastWest, northSouth) (-waypointNorth, waypointEast)
                | 180L -> runNextInstructions (eastWest, northSouth) (-waypointEast, -waypointNorth)
                | 270L -> runNextInstructions (eastWest, northSouth) (waypointNorth, -waypointEast)
            | "R" ->
                match value with
                | 90L -> runNextInstructions (eastWest, northSouth) (waypointNorth, -waypointEast)
                | 180L -> runNextInstructions (eastWest, northSouth) (-waypointEast, -waypointNorth)
                | 270L -> runNextInstructions (eastWest, northSouth) (-waypointNorth, waypointEast)
            | "F" ->
                runNextInstructions (eastWest + (value * waypointEast), northSouth + (value * waypointNorth)) (waypointEast, waypointNorth)

    runInstructions lines 0 (0L, 0L) (10L, 1L)


let GetManhattanDistanceAfterInstructions lines runInstructions =
    let parsedLines = lines |> Seq.map ParseInstruction
    let (eastWest, northSouth) = runInstructions parsedLines

    abs eastWest + abs northSouth


let SolvePuzzle lines =
    GetManhattanDistanceAfterInstructions lines RunAllInstructions

let SolvePuzzlePartTwo lines =
    GetManhattanDistanceAfterInstructions lines RunAllInstructionsWithWaypoint


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
