open System.IO

let rec BinarySearch (instructions: char list) (min, max) (upperChar, lowerChar) =
    match instructions with
    | head :: tail ->
        let range = max - min
        match head with
        | char when char = lowerChar ->
            let newMax = max - ((range + 1) / 2)
            BinarySearch tail (min, newMax) (upperChar, lowerChar)
        | char when char = upperChar ->
            let newMin = max - (range / 2)
            BinarySearch tail (newMin, max) (upperChar, lowerChar)
    | [] when min = max -> min

let GetRowPosition (fontBackInstructions: char list) (min, max) =
    BinarySearch fontBackInstructions (min, max) ('B', 'F')

let GetColumnPosition (rightLeftInstructions: char list) (min, max)  =
    BinarySearch rightLeftInstructions (min, max) ('R', 'L')

let GetSeatPosition (boardingPass: string) =
    let fontBackInstructions = List.ofSeq (boardingPass.Substring(0, 7))
    let row = GetRowPosition fontBackInstructions (0, 127)

    let rightLeftInstructions = List.ofSeq (boardingPass.Substring(7, 3))
    let column = GetColumnPosition rightLeftInstructions (0, 7)

    (row, column)

let GetSeatId (row, column) =
    (row * 8) + column

let SolvePuzzle boardingPasses =
    (Seq.map (GetSeatPosition >> GetSeatId) boardingPasses)
    |> Seq.max

[<EntryPoint>]
let main argv =

    let boardingPasses =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously

    printfn "Answer for part one is %d" (SolvePuzzle boardingPasses)

    0
