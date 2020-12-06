open System.IO

let rec CountTreesInPath (map: list<string>) (x, y) acc =
    if y < Seq.length map then
        let position = map.[y].[x]
        let newTreeCount =
            acc +
            match position with
            | '#' -> 1
            | _ -> 0

        let newY = y + 1
        let newX = (x + 3) % Seq.length map.[y]

        newTreeCount + CountTreesInPath map (newX, newY) acc
    else
        0

let SolvePuzzle map =
    CountTreesInPath map (0, 0) 0


[<EntryPoint>]
let main argv =

    let answer =
        File.ReadLines(argv.[0])
        |> Seq.toList
        |> SolvePuzzle

    printfn "Answer is %d" answer

    0
