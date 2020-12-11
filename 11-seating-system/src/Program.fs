open System.IO

type Seat =
    | Occupied
    | Empty
    | Floor

type Chart = list<list<Seat>>

let ParseSeatingChart (lines: string list): Chart =
    lines
    |> List.map (fun line ->
        line
        |> Seq.map (fun char ->
            match char with
            | 'L' -> Empty
            | '#' -> Occupied
            | '.' -> Floor
        )
        |> Seq.toList
    )

let GetSeat (chart: Chart) (lineIndex: int, columnIndex: int): option<Seat> =
    match chart |> List.tryItem lineIndex with
    | Some line ->
        match line |> List.tryItem columnIndex with
        | Some seat -> Some seat
        | None -> None
    | None -> None

let ApplyRules (chart: Chart): Chart =
    chart
    |> List.indexed
    |> List.map (fun (lineIndex, line) ->
        line
        |> List.indexed
        |> List.map (fun (columnIndex, seat) ->
            let left = GetSeat chart (lineIndex, columnIndex - 1)
            let right = GetSeat chart (lineIndex, columnIndex + 1)

            let up = GetSeat chart (lineIndex - 1, columnIndex)
            let upLeft = GetSeat chart (lineIndex - 1, columnIndex - 1)
            let upRight = GetSeat chart (lineIndex - 1, columnIndex + 1)

            let down = GetSeat chart (lineIndex + 1, columnIndex)
            let downLeft = GetSeat chart (lineIndex + 1, columnIndex - 1)
            let downRight = GetSeat chart (lineIndex + 1, columnIndex + 1)

            let adjacents = [left; right; up; down; upLeft; upRight; downLeft; downRight]

            match seat with
            | Empty ->
                match adjacents |> Seq.contains (Some Occupied) with
                | true -> Empty
                | false -> Occupied
            | Occupied ->
                match adjacents |> Seq.filter ((=) (Some Occupied)) |> Seq.length with
                | occupied when occupied >= 4 -> Empty
                | _ -> Occupied
            | Floor -> Floor
        )
    )

let ApplyRulesPartTwo (chart: Chart): Chart =
    chart
    |> List.indexed
    |> List.map (fun (lineIndex, line) ->
        line
        |> List.indexed
        |> List.map (fun (columnIndex, seat) ->
            let rec findFirstSeat lineI columnI transform =
                let (lineToCheck, columnToCheck) = transform (lineI, columnI)

                let seatToCheck = GetSeat chart (lineToCheck, columnToCheck)

                match seatToCheck with
                | Some s when s <> Floor -> Some s
                | Some _ -> findFirstSeat lineToCheck columnToCheck transform
                | None -> None

            let left = findFirstSeat lineIndex columnIndex (fun (lineI, columnI) -> (lineI, columnI - 1))
            let right = findFirstSeat lineIndex columnIndex (fun (lineI, columnI) -> (lineI, columnI + 1))

            let up = findFirstSeat lineIndex columnIndex (fun (lineI, columnI) -> (lineI - 1, columnI))
            let upLeft = findFirstSeat lineIndex columnIndex (fun (lineI, columnI) -> (lineI - 1, columnI - 1))
            let upRight = findFirstSeat lineIndex columnIndex (fun (lineI, columnI) -> (lineI - 1, columnI + 1))

            let down = findFirstSeat lineIndex columnIndex (fun (lineI, columnI) -> (lineI + 1, columnI))
            let downLeft = findFirstSeat lineIndex columnIndex (fun (lineI, columnI) -> (lineI + 1, columnI - 1))
            let downRight = findFirstSeat lineIndex columnIndex (fun (lineI, columnI) -> (lineI + 1, columnI + 1))

            let adjacents = [left; right; up; down; upLeft; upRight; downLeft; downRight]

            match seat with
            | Empty ->
                match adjacents |> Seq.contains (Some Occupied) with
                | true -> Empty
                | false -> Occupied
            | Occupied ->
                match adjacents |> Seq.filter ((=) (Some Occupied)) |> Seq.length with
                | occupied when occupied >= 5 -> Empty
                | _ -> Occupied
            | Floor -> Floor
        )
    )


let CountSeats (chart: Chart) (seat: Seat) =
    chart |> List.sumBy (fun line ->
        line
        |> List.filter ((=) seat)
        |> List.length
    )

let rec CountOccupiedSeatsAfterStability applyRules chart =
    let newChart = applyRules chart

    if newChart = chart then
        CountSeats newChart Occupied
    else
        CountOccupiedSeatsAfterStability applyRules newChart

let SolvePuzzle lines =
    let chart = ParseSeatingChart lines
    CountOccupiedSeatsAfterStability ApplyRules chart

let SolvePuzzlePartTwo lines =
    let chart = ParseSeatingChart lines
    CountOccupiedSeatsAfterStability ApplyRulesPartTwo chart

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
