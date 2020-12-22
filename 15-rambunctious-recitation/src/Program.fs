type SpokenNumber = { LastTurnSpoken: int; LastLastTurnSpoken: option<int> }

let rec SpeakNumbersUntilTurn turnToStopAt allSpokenNumbers lastNumber =
    let lastSpokenNumber = allSpokenNumbers |> Map.find lastNumber

    if lastSpokenNumber.LastTurnSpoken = turnToStopAt then
        lastNumber
    else
        let currentTurn = lastSpokenNumber.LastTurnSpoken + 1
        let numberToSpeak =
            match lastSpokenNumber.LastLastTurnSpoken with
            | Some lastLastTurnSpoken -> lastSpokenNumber.LastTurnSpoken - lastLastTurnSpoken
            | None -> 0

        let newSpokenNumbers = allSpokenNumbers |> Map.change numberToSpeak (fun toChange ->
            match toChange with
            | Some turn ->
                Some ({ LastTurnSpoken = currentTurn; LastLastTurnSpoken = Some turn.LastTurnSpoken })
            | None ->
                Some ({ LastTurnSpoken = currentTurn; LastLastTurnSpoken = None })
        )

        SpeakNumbersUntilTurn turnToStopAt newSpokenNumbers numberToSpeak


let SpeakNumbers initialNumbers turnToStopAt =
    let spokenNumbers: Map<int, SpokenNumber> =
        initialNumbers
        |> Array.indexed
        |> Array.map (fun (index, number) ->
            (number, { LastTurnSpoken = index + 1; LastLastTurnSpoken = None })
        )
        |> Map.ofArray

    SpeakNumbersUntilTurn turnToStopAt spokenNumbers (initialNumbers |> Seq.last)


let SolvePuzzle (input: string) =
    let numbers = input.Split(',') |> Array.map int

    SpeakNumbers numbers 2020

[<EntryPoint>]
let main argv =

    printfn "Answer for part one is %d" (SolvePuzzle "0,5,4,1,10,14,7")

    0
