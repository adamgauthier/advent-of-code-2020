open System.IO

let rec GetAllPairs l =
    seq {
        match l with
        | head :: tail ->
            for element in tail do
                yield (head, element)
            yield! GetAllPairs tail
        | _ -> ()
    }

let FindPairThatSumsTo sumTo numbers =
    GetAllPairs numbers
    |> Seq.find (fun (firstNumber, secondNumber) -> firstNumber + secondNumber = sumTo)

let SolvePuzzle numbers = 
    let (a, b) = FindPairThatSumsTo 2020 numbers
    (a * b)

[<EntryPoint>]
let main argv =

    let numbers =
        File.ReadLines(argv.[0])
        |> Seq.map int
        |> Seq.toList

    let answer = SolvePuzzle numbers
    printfn "Answer is %d" answer

    0
