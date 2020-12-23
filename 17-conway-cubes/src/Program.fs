open System.IO

type Cube =
    | Active
    | Inactive

type Dimension =
    | CubeDimension of Map<int, Cube>
    | WrappedDimension of Map<int, Dimension>

let ParseDimension2D (lines: string list) =
    let parsedDimension =
        lines
        |> List.map (fun line ->
            let parsedCubes = line |> Seq.map (fun c ->
                match c with
                | '#' -> Active
                | '.' -> Inactive
            )

            CubeDimension (parsedCubes |> Seq.indexed |> Map.ofSeq)
        )
        |> List.indexed
        |> Map.ofList

    WrappedDimension parsedDimension

let rec GetCube (dimension: Dimension) (indexes: int list): Cube =
    let firstIndex :: rest = indexes

    match dimension with
    | CubeDimension cubeDimension ->
        match cubeDimension |> Map.tryFind firstIndex with
        | Some cube -> cube
        | None -> Inactive
    | WrappedDimension wrappedDimension ->
        match wrappedDimension |> Map.tryFind firstIndex with
        | Some subDimension -> GetCube subDimension rest
        | None -> Inactive

let rec CountCubes (dimension: Dimension) (cubeType: Cube) =
    match dimension with
    | CubeDimension cubeDimension ->
        cubeDimension
        |> Map.filter (fun _ value -> value = cubeType)
        |> Map.count
    | WrappedDimension wrappedDimension ->
        wrappedDimension
        |> Map.toList
        |> List.sumBy (fun (_, subDimension) -> CountCubes subDimension cubeType)

let GetAdjacents (indexes: int list): int list list =
    let rec loop indexesLeft currentIndexes =
        match indexesLeft with
        | lastIndex :: [] ->
            [lastIndex-1..lastIndex+1] |> List.map (fun i -> currentIndexes @ [i])
        | firstRemainingIndex :: rest ->
            [firstRemainingIndex-1..firstRemainingIndex+1] |> List.collect (fun i -> loop (rest) (currentIndexes @ [i]))

    loop (indexes) []

let rec ExtendDimension (dimension: Dimension) =
    match dimension with
    | CubeDimension cubeDimension ->
        let lowestIndex = cubeDimension |> Map.toList |> List.map fst |> List.min
        let highestIndex = cubeDimension |> Map.toList |> List.map fst |> List.max

        let extended =
            cubeDimension
            |> Map.add (lowestIndex-1) Inactive
            |> Map.add (highestIndex+1) Inactive

        CubeDimension extended
    | WrappedDimension wrappedDimension ->
        let extendedSubDimension =
            wrappedDimension
            |> Map.map (fun _ subDimension -> ExtendDimension subDimension)

        let rec createInactiveDimension (dim: Dimension) =
            match dim with
            | CubeDimension cubeDimension ->
                CubeDimension (cubeDimension |> Map.map (fun _ _ -> Inactive))
            | WrappedDimension wrappedDimension ->
                WrappedDimension (wrappedDimension |> Map.map (fun _ d -> d |> createInactiveDimension))

        let lowestIndex = extendedSubDimension |> Map.toList |> List.map fst |> List.min
        let highestIndex = extendedSubDimension |> Map.toList |> List.map fst |> List.max

        let extended =
            extendedSubDimension
            |> Map.add (lowestIndex-1) (extendedSubDimension |> Map.find lowestIndex |> createInactiveDimension)
            |> Map.add (highestIndex+1) (extendedSubDimension |> Map.find highestIndex |> createInactiveDimension)

        WrappedDimension extended

let ApplyRules (dimension: Dimension): Dimension =
    let extended = ExtendDimension dimension

    let rec transformCubes dim indexes =
        match dim with
        | CubeDimension cubeDimension ->
            CubeDimension (cubeDimension |> Map.map (fun lastIndex cube ->
                let coordinates = (indexes @ [lastIndex])

                let adjacents =
                    GetAdjacents coordinates
                    |> Seq.except [coordinates]
                    |> Seq.map (GetCube extended)
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
            ))
        | WrappedDimension wrappedDimension ->
            WrappedDimension (wrappedDimension |> Map.map (fun index d ->
                transformCubes d (indexes @ [index])
            ))

    transformCubes extended []


let rec ApplySixCycles dimension i =
    if i = 6 then
        dimension
    else
        let newDimension = ApplyRules dimension
        ApplySixCycles newDimension (i+1)

let SolvePuzzle lines =
    let dimension2D = ParseDimension2D lines
    let dimension3D = WrappedDimension (Map.ofList [(0, dimension2D)])

    let afterSixCycles = ApplySixCycles dimension3D 0

    CountCubes afterSixCycles Active

let SolvePuzzlePartTwo lines =
    let dimension2D = ParseDimension2D lines
    let dimension3D = WrappedDimension (Map.ofList [(0, dimension2D)])
    let dimension4D = WrappedDimension (Map.ofList [(0, dimension3D)])

    let afterSixCycles = ApplySixCycles dimension4D 0

    CountCubes afterSixCycles Active

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
