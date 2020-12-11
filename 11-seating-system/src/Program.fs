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
                | true -> seat
                | false -> Occupied
            | Occupied ->
                match adjacents |> Seq.filter ((=) (Some Occupied)) |> Seq.length with
                | occupied when occupied >= 4 -> Empty
                | _ -> seat
            | Floor -> Floor
        )
    )

let rec ApplyRulesUntilStability chart =
    let newChart = ApplyRules chart

    if newChart = chart then
        newChart
    else
        ApplyRulesUntilStability newChart

let CountSeats (chart: Chart) (seat: Seat) =
    chart |> List.sumBy (fun line ->
        line
        |> List.filter ((=) seat)
        |> List.length
    )

let SolvePuzzle lines =
    let chart = ParseSeatingChart lines
    let stabilizedChart = ApplyRulesUntilStability chart

    CountSeats stabilizedChart Occupied


[<EntryPoint>]
let main argv =

    let lines =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer for part one is %d" (SolvePuzzle lines)

    0
