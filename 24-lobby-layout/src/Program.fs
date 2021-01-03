open System.IO

type Direction = | East | SouthEast | SouthWest | West | NorthWest | NorthEast

type TilePath = Direction list

type DirectionMapping = { String: string; Direction: Direction }

let mappings = [
    { String = "e"; Direction = East }
    { String = "se"; Direction = SouthEast }
    { String = "sw"; Direction = SouthWest }
    { String = "w"; Direction = West }
    { String = "nw"; Direction = NorthWest }
    { String = "ne"; Direction = NorthEast }
]

let ParseTilePath (tilePath: string): TilePath =
    let rec parse parsed remaining =
        if remaining = "" then
            parsed
        else
            let matching = mappings |> List.find (fun mapping -> remaining.StartsWith(mapping.String))

            parse (parsed @ [matching.Direction]) (remaining.Substring(matching.String.Length))

    parse [] tilePath

type TileCoordinate = (int * int * int)

let GetOffset direction =
    match direction with
    | East -> (1, -1, 0)
    | SouthEast -> (0, -1, 1)
    | SouthWest -> (-1, 0, 1)
    | West -> (-1, 1, 0)
    | NorthWest -> (0, 1, -1)
    | NorthEast -> (1, 0, -1)

let ToCoordinate (tilePath: TilePath): TileCoordinate =
    tilePath
    |> List.map GetOffset
    |> List.fold (fun (x, y, z) (xOffset, yOffset, zOffset) -> (x + xOffset, y + yOffset, z + zOffset)) (0, 0, 0)

let FlipTiles (tiles: TileCoordinate list) =
    let rec flip (blackTiles: TileCoordinate list) (remainingTiles: TileCoordinate list) =
        match remainingTiles with
        | [] -> blackTiles
        | remaining :: rest ->
            if blackTiles |> List.contains remaining then
                flip (blackTiles |> List.except [remaining]) rest
            else
                flip (blackTiles @ [remaining]) rest

    flip [] tiles

let SolvePuzzle input =
    let tilePaths =
        input |> List.map (ParseTilePath >> ToCoordinate)

    let blackTiles = FlipTiles tilePaths

    List.length blackTiles

[<EntryPoint>]
let main argv =

    let input =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer for part one is %d" (SolvePuzzle input)

    0
