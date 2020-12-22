open System.IO

type Cube =
    | Active
    | Inactive

type Chart2D = Map<int, Map<int, Cube>>
type Chart3D = Map<int, Chart2D>

let ParseCubeChart (lines: string list): Chart2D =
    lines
    |> List.map (fun line ->
        line
        |> Seq.map (fun c ->
            match c with
            | '#' -> Active
            | '.' -> Inactive
        )
        |> Seq.indexed
        |> Map.ofSeq
    )
    |> List.indexed
    |> Map.ofList

let GetCube (chart: Chart3D) (zIndex: int, lineIndex: int, columnIndex: int): Cube =
    match chart |> Map.tryFind zIndex with
    | Some dimension ->
        match dimension |> Map.tryFind lineIndex with
        | Some line ->
            match line |> Map.tryFind columnIndex with
            | Some cube -> cube
            | None -> Inactive
        | None -> Inactive
    | None -> Inactive

let rec GetAdjacents z x y =
    seq {
        for zAdj in [(z-1)..(z+1)] do
            for xAdj in [(x-1)..(x+1)] do
                for yAdj in [(y-1)..(y+1)] do
                    yield (zAdj, xAdj, yAdj)
    }

let ExtendChard3D chart =
    let lowestZ = chart |> Map.toList |> List.map fst |> List.min
    let highestZ = chart |> Map.toList |> List.map fst |> List.max

    let extended =
        chart
        |> Map.map (fun _ dimension ->
            let lowestX = dimension |> Map.toList |> List.map fst |> List.min
            let highestX = dimension |> Map.toList |> List.map fst |> List.max

            let extendedDimension =
                dimension
                |> Map.map (fun _ line ->
                    let lowestY = line |> Map.toList |> List.map fst |> List.min
                    let highestY = line |> Map.toList |> List.map fst |> List.max
                    line
                    |> Map.add (lowestY-1) Inactive
                    |> Map.add (highestY+1) Inactive
                )

            extendedDimension
            |> Map.add (lowestX-1) (extendedDimension |> Map.find lowestX |> Map.map (fun _ _ -> Inactive))
            |> Map.add (highestX+1) (extendedDimension |> Map.find highestX |> Map.map (fun _ _ -> Inactive))
        )

    extended
    |> Map.add (lowestZ-1) (extended |> Map.find lowestZ |> Map.map (fun _ d -> d |> Map.map (fun _ _ -> Inactive)))
    |> Map.add (highestZ+1) (extended |> Map.find highestZ |> Map.map (fun _ d -> d |> Map.map (fun _ _ -> Inactive)))

let ApplyRules (chart: Chart3D): Chart3D =
    let extended = ExtendChard3D chart

    extended
    |> Map.map (fun zIndex dimension ->
        dimension
        |> Map.map (fun lineIndex line ->
            line
            |> Map.map (fun columnIndex cube ->
                let adjacents =
                    GetAdjacents zIndex lineIndex columnIndex
                    |> Seq.filter ((<>) (zIndex, lineIndex, columnIndex))
                    |> Seq.map (GetCube chart)
                    |> List.ofSeq

                match cube with
                | Active ->
                    match adjacents |> Seq.filter ((=) Active) |> Seq.length with
                    | active when active = 2 || active = 3 -> Active
                    | _ -> Inactive
                | Inactive ->
                    match adjacents |> Seq.filter ((=) Active) |> Seq.length with
                    | active when active = 3 -> Active
                    | _ -> Inactive
            )
        )
    )

let CountCubes (chart: Chart3D) (cube: Cube) =
    chart
    |> Map.toList
    |> List.sumBy (fun (_, dimension) ->
        dimension
        |> Map.toList
        |> List.sumBy (fun (_, line) ->
            line
            |> Map.filter (fun _ value -> value = cube)
            |> Map.count
        )
    )

let rec ApplySixCycles applyRules chart i =
    if i = 6 then
        chart
    else
        let newChart = applyRules chart
        ApplySixCycles applyRules newChart (i+1)

let SolvePuzzle lines =
    let chart = ParseCubeChart lines
    let chart3D = Map.empty |> Map.add 0 chart

    let afterSixCycles = ApplySixCycles ApplyRules chart3D 0

    CountCubes afterSixCycles Active

[<EntryPoint>]
let main argv =

    let lines =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer for part one is %d" (SolvePuzzle lines)

    0
