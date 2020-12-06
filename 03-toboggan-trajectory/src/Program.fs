open System.IO

let rec CountTrees (layout: list<string>) (x, y) (moveX, moveY) acc =
    if y < Seq.length layout then
        let position = layout.[y].[x]
        let newTreeCount =
            acc +
            match position with
            | '#' -> 1
            | _ -> 0

        let newY = y + moveY
        let newX = (x + moveX) % (Seq.length layout.[y])

        newTreeCount + CountTrees layout (newX, newY) (moveX, moveY) acc
    else
        0

let CountTreesInPath layout (moveX, moveY) =
    CountTrees layout (0, 0) (moveX, moveY) 0

let SolvePuzzle layout =
    CountTreesInPath layout (3, 1)

let GetProduct (numbers) =
    Seq.fold Checked.(*) 1UL numbers

let SolvePuzzlePartTwo layout =
    let countToUint64 = ((CountTreesInPath layout) >> uint64)
    Seq.map countToUint64 [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]
    |> GetProduct


[<EntryPoint>]
let main argv =

    let layout =
        File.ReadLines(argv.[0])
        |> Seq.toList

    printfn "Answer for part one is %d" (SolvePuzzle layout)
    printfn "Answer for part two is %d" (SolvePuzzlePartTwo layout)

    0
