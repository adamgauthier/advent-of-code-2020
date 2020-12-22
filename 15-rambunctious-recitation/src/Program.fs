type LastSpoken = { LastTurnSpoken: int; LastLastTurnSpoken: option<int> }

// using mutable data structure to dramatically improve peformance
let rec SpeakNumbersUntilTurn turnToStopAt (spokenNumbers: System.Collections.Generic.Dictionary<int, LastSpoken>) lastNumber =
    let lastSpokenNumber = spokenNumbers.[lastNumber]

    if lastSpokenNumber.LastTurnSpoken = turnToStopAt then
        lastNumber
    else
        let currentTurn = lastSpokenNumber.LastTurnSpoken + 1
        let numberToSpeak =
            match lastSpokenNumber.LastLastTurnSpoken with
            | Some lastLastTurnSpoken -> lastSpokenNumber.LastTurnSpoken - lastLastTurnSpoken
            | None -> 0

        let lastLastTurnSpoken =
            match spokenNumbers.TryGetValue(numberToSpeak) with
            | true, value -> Some value.LastTurnSpoken
            | false, _ -> None

        spokenNumbers.[numberToSpeak] <- { LastTurnSpoken = currentTurn; LastLastTurnSpoken = lastLastTurnSpoken }

        SpeakNumbersUntilTurn turnToStopAt spokenNumbers numberToSpeak


let SpeakNumbers initialNumbers turnToStopAt =
    let spokenNumbers = new System.Collections.Generic.Dictionary<int, LastSpoken>()

    initialNumbers
    |> Array.indexed
    |> Array.iter (fun (index, number) ->
        spokenNumbers.[number] <- { LastTurnSpoken = index + 1; LastLastTurnSpoken = None }
    )

    SpeakNumbersUntilTurn turnToStopAt spokenNumbers (initialNumbers |> Seq.last)


let SolvePuzzle (input: string) =
    let numbers = input.Split(',') |> Array.map int

    SpeakNumbers numbers 2020

let SolvePuzzlePartTwo (input: string) =
    let numbers = input.Split(',') |> Array.map int

    SpeakNumbers numbers 30000000

[<EntryPoint>]
let main argv =

    printfn "Answer for part one is %d" (SolvePuzzle "0,5,4,1,10,14,7")
    printfn "Answer for part two is %d" (SolvePuzzlePartTwo "0,5,4,1,10,14,7")

    0
