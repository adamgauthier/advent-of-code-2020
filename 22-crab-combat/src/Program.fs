open System.IO

let ParseInput (input: string) =
    let decks = input.Trim('\n').Split("\n\n")

    decks
    |> Array.toList
    |> List.map (fun rawDeck ->
        let _ :: cards = rawDeck.Split("\n") |> Array.toList
        cards |> List.map int
    )

let GetScore deck =
    deck
    |> List.rev
    |> List.indexed
    |> List.sumBy (fun (index, card) ->
        (index + 1) * card
    )

type Winner = { WinningPlayer: int; WinningDeck: int list }

let rec PlayGameUntilWinner firstPlayerDeck secondPlayerDeck =
    if List.isEmpty firstPlayerDeck then
        { WinningPlayer = 2; WinningDeck = secondPlayerDeck }
    elif List.isEmpty secondPlayerDeck then
        { WinningPlayer = 1; WinningDeck = firstPlayerDeck }
    else
        let firstPlayerFirstCard :: firstPlayerRest = firstPlayerDeck
        let secondPlayerFirstCard :: secondPlayerRest = secondPlayerDeck

        if firstPlayerFirstCard > secondPlayerFirstCard then
            PlayGameUntilWinner (firstPlayerRest @ [firstPlayerFirstCard; secondPlayerFirstCard]) secondPlayerRest
        else
            PlayGameUntilWinner firstPlayerRest (secondPlayerRest @ [secondPlayerFirstCard; firstPlayerFirstCard])

let SolvePuzzle (input: string) =
    let [ firstPlayerDeck; secondPlayerDeck ] = ParseInput input

    let winner = PlayGameUntilWinner firstPlayerDeck secondPlayerDeck

    GetScore winner.WinningDeck

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
