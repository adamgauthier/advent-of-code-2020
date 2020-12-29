open System.IO

type TileId = int64
type TileImage = Map<(int * int), char>
type Tile = { Id: TileId; Size: int; Image: TileImage }

let ParseInput (input: string): Tile list =
    let tiles = input.Trim('\n').Split("\n\n")

    tiles
    |> Array.toList
    |> List.map (fun tile ->
        let idLine :: imageRows = (tile.Split("\n") |> Array.toList)

        let tileImage =
            imageRows
            |> List.indexed
            |> List.collect (fun (rowIndex, row) ->
                row
                |> Seq.indexed
                |> Seq.map (fun (columnIndex, character) ->
                    let position = (rowIndex, columnIndex)
                    (position, character)
                )
                |> Seq.toList
            )
            |> Map.ofList

        let [| _; numberText |] = idLine.Split(" ")
        let tileId = numberText.[0..numberText.Length-2] |> int64

        { Id=tileId; Size=imageRows.Length; Image=tileImage }
    )

let GetTopRow (tile: Tile) =
    tile.Image
    |> Map.filter (fun (rowIndex, columnIndex) value -> rowIndex = 0)
    |> Map.toList
    |> List.map (fun ((_, columnIndex), value) -> (columnIndex, value))
    |> Map.ofList

let GetBottomRow (tile: Tile) =
    tile.Image
    |> Map.filter (fun (rowIndex, columnIndex) value -> rowIndex = tile.Size - 1)
    |> Map.toList
    |> List.map (fun ((_, columnIndex), value) -> (columnIndex, value))
    |> Map.ofList

let GetLeftColumn (tile: Tile) =
    tile.Image
    |> Map.filter (fun (rowIndex, columnIndex) value -> columnIndex = 0)
    |> Map.toList
    |> List.map (fun ((rowIndex, _), value) -> (rowIndex, value))
    |> Map.ofList

let GetRightColumn (tile: Tile) =
    tile.Image
    |> Map.filter (fun (rowIndex, columnIndex) value -> columnIndex = tile.Size - 1)
    |> Map.toList
    |> List.map (fun ((rowIndex, _), value) -> (rowIndex, value))
    |> Map.ofList

let RotateLeft (tile: Tile): Tile =
    let rotatedImage =
        tile.Image
        |> Map.map (fun (rowIndex, columnIndex) _ ->
            tile.Image.[(columnIndex, (tile.Size - 1) - rowIndex)]
        )

    { tile with Image = rotatedImage }

let FlipHorizontal (tile: Tile): Tile =
    let flippedImage =
        tile.Image
        |> Map.map (fun (rowIndex, columnIndex) _ ->
            tile.Image.[(rowIndex, (tile.Size - 1) - columnIndex)]
        )

    { tile with Image = flippedImage }

let GetTileTransforms (tile: Tile): seq<Tile> =
    seq {
        yield tile
        yield (FlipHorizontal tile)

        let rotatedOnce = RotateLeft tile
        yield rotatedOnce
        yield (FlipHorizontal rotatedOnce)

        let rotatedTwice = RotateLeft rotatedOnce
        yield rotatedTwice
        yield (FlipHorizontal rotatedTwice)

        let rotatedThrice = RotateLeft rotatedTwice
        yield rotatedThrice
        yield (FlipHorizontal rotatedThrice)
    }

let FindTopLeftCorner (tiles: Tile list) =
    tiles
    |> List.filter (fun tile ->
        let otherTiles = tiles |> List.except [tile]

        let topRow = GetTopRow tile
        let leftColumn = GetLeftColumn tile

        let topRowHasMatch =
            otherTiles
            |> List.exists (fun otherTile ->
                let otherTileTransforms = GetTileTransforms otherTile

                otherTileTransforms
                |> Seq.exists (fun transform ->
                    topRow = GetBottomRow transform
                )
            )

        let leftColumnHasMatch =
            otherTiles
            |> List.exists (fun otherTile ->
                let otherTileTransforms = GetTileTransforms otherTile

                otherTileTransforms
                |> Seq.exists (fun transform ->
                    leftColumn = GetRightColumn transform
                )
            )

        not topRowHasMatch && not leftColumnHasMatch
    )
    |> List.exactlyOne

type SquareSolution = {
    Size: int
    LastSolved: int * int
    Map: Map<int * int, (Tile)>
}

let rec ArrangeTiles (remainingTileTransforms: Tile list) (solution: SquareSolution) =
    if List.isEmpty remainingTileTransforms then
        solution
    else
        let (lastSolvedRow, lastSolvedColumn) = solution.LastSolved

        let (toSolveRow, toSolveColumn) =
            if lastSolvedColumn + 1 >= solution.Size then
                (lastSolvedRow + 1, 0)
            else
                (lastSolvedRow, lastSolvedColumn + 1)

        let matchesLeftTile transform =
            match solution.Map |> Map.tryFind (toSolveRow, toSolveColumn - 1) with
            | Some leftTile ->
                let rightBorder = GetRightColumn leftTile
                rightBorder = GetLeftColumn transform
            | None -> true

        let matchesUpTile transform =
            match solution.Map |> Map.tryFind (toSolveRow - 1, toSolveColumn) with
            | Some upTile ->
                let bottomBorder = GetBottomRow upTile
                bottomBorder = GetTopRow transform
            | None -> true

        let matchingTile =
            remainingTileTransforms
            |> Seq.filter (fun tileTransform ->
                matchesLeftTile tileTransform &&
                matchesUpTile tileTransform
            )
            |> Seq.exactlyOne

        ArrangeTiles
            (remainingTileTransforms |> List.filter (fun tile -> tile.Id <> matchingTile.Id))
            ({
                solution with
                    Map = solution.Map |> Map.change (toSolveRow, toSolveColumn) (fun (None) -> Some matchingTile)
                    LastSolved = (toSolveRow, toSolveColumn)
            })

let ArrangeAllTiles tiles =
    let topLeftCorner = FindTopLeftCorner tiles

    ArrangeTiles
        (tiles |> List.except [topLeftCorner] |> Seq.collect GetTileTransforms |> Seq.toList)
        ({
            Size = int (sqrt (float (List.length tiles)))
            LastSolved = 0, 0
            Map = [((0,0), topLeftCorner)] |> Map.ofList
        })

let SolvePuzzle input =
    let tiles = ParseInput input

    let solution = ArrangeAllTiles tiles

    let cornerIds = [
        solution.Map.[0, 0].Id
        solution.Map.[0, solution.Size - 1].Id
        solution.Map.[solution.Size - 1, 0].Id
        solution.Map.[solution.Size - 1, solution.Size - 1].Id
    ]

    cornerIds
    |> Seq.fold Checked.(*) 1L

[<EntryPoint>]
let main argv =

    let input =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray
        |> String.concat "\n"

    printfn "Answer for part one is %d" (SolvePuzzle input)

    0
