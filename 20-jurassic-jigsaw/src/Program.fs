open System.IO
open System
open System.Text.RegularExpressions

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


type TileWithSeaMonsterCount = { Tile: Tile; SeaMonsterCount: int }

let TileToStringList tile =
    tile.Image
    |> Map.fold (fun newMap (rowIndex, columnIndex) character ->
        newMap |> Map.change rowIndex (fun currentRow ->
            match currentRow with
            | Some row -> Some (row |> (Map.add columnIndex character))
            | None -> Some ([(columnIndex, character)] |> Map.ofList)
        )
    ) Map.empty
    |> Map.map (fun position value ->
        value
        |> Map.toList
        |> List.map snd
        |> Seq.toArray
        |> String
    )
    |> Map.toList
    |> List.map snd

let SolvePuzzlePartTwo input =
    let tiles = ParseInput input

    let solution = ArrangeAllTiles tiles

    let withoutBorders =
        solution.Map
        |> Map.map (fun position tile ->
            let imageWithoutBorder =
                tile.Image
                |> Map.filter (fun (rowIndex, columnIndex) character ->
                    rowIndex <> 0 && rowIndex <> tile.Size - 1 &&
                    columnIndex <> 0 && columnIndex <> tile.Size - 1
                )
                |> Map.toList
                |> List.map (fun ((rowIndex, columnIndex), character) ->
                    ((rowIndex - 1, columnIndex - 1), character)
                )
                |> Map.ofList

            let newSize = tile.Size - 2

            { tile with Image = imageWithoutBorder; Size = newSize }
        )

    let unifiedTile = {
        Id = 0L
        Size = solution.Size * withoutBorders.[0, 0].Size
        Image =
            withoutBorders
            |> Map.toList
            |> List.collect (fun ((tileRowIndex, tileColumnIndex), tile) ->
                tile.Image
                |> Map.toList
                |> List.map (fun ((rowIndex, columnIndex), character) ->
                    let newPosition = (
                        (tileRowIndex * tile.Size) + rowIndex,
                        (tileColumnIndex * tile.Size) + columnIndex
                    )
                    (newPosition, character)
                )
            )
            |> Map.ofList
    }

    let seaMonsterPattern =
        [
            "..................#."
            "#....##....##....###"
            ".#..#..#..#..#..#..."
        ] |> List.map Regex

    let transformWithMostSeaMonsters =
        GetTileTransforms unifiedTile
        |> Seq.map (fun transformedTile ->
            let tileAsList = TileToStringList transformedTile
            let tileWithoutTopAndBottom = tileAsList.[1..tileAsList.Length-2]

            let seaMonsterCount =
                tileWithoutTopAndBottom
                |> List.indexed
                |> List.fold (fun count (index, line) ->
                    let seaMonsterMiddleLine = seaMonsterPattern.[1]

                    let seaMonsterCountInThisLine =
                        seaMonsterMiddleLine.Matches(line)
                        |> Seq.filter (fun aMatch ->
                            let previousLine = tileWithoutTopAndBottom.[index - 1].[aMatch.Index .. aMatch.Index + aMatch.Value.Length]
                            let nextLine = tileWithoutTopAndBottom.[index + 1].[aMatch.Index .. aMatch.Index + aMatch.Value.Length]

                            seaMonsterPattern.[0].IsMatch(previousLine) &&
                            seaMonsterPattern.[2].IsMatch(nextLine)
                        )
                        |> Seq.length

                    count + seaMonsterCountInThisLine
                ) 0

            { Tile=transformedTile; SeaMonsterCount=seaMonsterCount }
        )
        |> Seq.maxBy (fun tileWithCount -> tileWithCount.SeaMonsterCount)

    let waterRoughness =
        let roughnessCount = transformWithMostSeaMonsters.Tile.Image |> Map.filter (fun _ character -> character = '#') |> Map.count
        roughnessCount - (transformWithMostSeaMonsters.SeaMonsterCount * 15)

    waterRoughness


[<EntryPoint>]
let main argv =

    let input =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray
        |> String.concat "\n"

    printfn "Answer for part one is %d" (SolvePuzzle input)
    printfn "Answer for part two is %d" (SolvePuzzlePartTwo input)

    0
