open System.IO

let GetProduct numbers =
    Seq.fold Checked.(*) 1 numbers

let SumsTo sum (numbers: seq<int>) =
    Seq.sum numbers = sum

let rec GetAllPairs l =
    seq {
        match l with
        | head :: tail ->
            for element in tail do
                yield [head; element]
            yield! GetAllPairs tail
        | _ -> ()
    }

let SolvePuzzle numbers =
    GetAllPairs numbers
    |> Seq.find (SumsTo 2020)
    |> GetProduct


let rec GetAllTriplets numbers =
    seq {
        match numbers with
        | head :: tail ->
            let tailPairs = GetAllPairs tail
            for pair in tailPairs do
                yield head :: pair
            yield! GetAllTriplets tail
        | _ -> ()
    }

let SolvePuzzlePartTwo numbers =
    GetAllTriplets numbers
    |> Seq.find (SumsTo 2020)
    |> GetProduct


[<EntryPoint>]
let main argv =

    let numbers =
        File.ReadLines(argv.[0])
        |> Seq.map int
        |> Seq.toList

    printfn "Answer for part one is %d" (SolvePuzzle numbers)
    printfn "Answer for part two is %d" (SolvePuzzlePartTwo numbers)

    0
