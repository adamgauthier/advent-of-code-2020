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

let rec PlayGamePartOne playerOneDeck playerTwoDeck =
    if List.isEmpty playerOneDeck then
        { WinningPlayer = 2; WinningDeck = playerTwoDeck }
    elif List.isEmpty playerTwoDeck then
        { WinningPlayer = 1; WinningDeck = playerOneDeck }
    else
        let playerOneFirstCard :: playerOneRest = playerOneDeck
        let playerTwoFirstCard :: playerTwoRest = playerTwoDeck

        if playerOneFirstCard > playerTwoFirstCard then
            PlayGamePartOne (playerOneRest @ [playerOneFirstCard; playerTwoFirstCard]) playerTwoRest
        else
            PlayGamePartOne playerOneRest (playerTwoRest @ [playerTwoFirstCard; playerOneFirstCard])

module RoundMemory =
    type Memory = { RoundsPlayerOne: int list list; RoundsPlayerTwo: int list list }

    let empty = { RoundsPlayerOne = []; RoundsPlayerTwo = [] }

    let hasBeenPlayedBefore playerOneDeck playerTwoDeck roundMemory =
        roundMemory.RoundsPlayerOne |> List.contains playerOneDeck ||
        roundMemory.RoundsPlayerTwo |> List.contains playerTwoDeck

let rec PlayGamePartTwo (roundMemory: RoundMemory.Memory) playerOneDeck playerTwoDeck =
    if roundMemory |> RoundMemory.hasBeenPlayedBefore playerOneDeck playerTwoDeck then
        { WinningPlayer = 1; WinningDeck = playerOneDeck }
    elif List.isEmpty playerOneDeck then
        { WinningPlayer = 2; WinningDeck = playerTwoDeck }
    elif List.isEmpty playerTwoDeck then
        { WinningPlayer = 1; WinningDeck = playerOneDeck }
    else
        let newMemory: RoundMemory.Memory = {
            RoundsPlayerOne = roundMemory.RoundsPlayerOne @ [playerOneDeck]
            RoundsPlayerTwo = roundMemory.RoundsPlayerTwo @ [playerTwoDeck]
        }

        let playNextTurn = PlayGamePartTwo newMemory

        let playerOneFirstCard :: playerOneRest = playerOneDeck
        let playerTwoFirstCard :: playerTwoRest = playerTwoDeck

        if playerOneRest.Length >= playerOneFirstCard && playerTwoRest.Length >= playerTwoFirstCard then
            let subGameWinner = PlayGamePartTwo RoundMemory.empty (playerOneRest |> List.take playerOneFirstCard) (playerTwoRest |> List.take playerTwoFirstCard)

            if subGameWinner.WinningPlayer = 1 then
                playNextTurn (playerOneRest @ [playerOneFirstCard; playerTwoFirstCard]) playerTwoRest
            else
                playNextTurn playerOneRest (playerTwoRest @ [playerTwoFirstCard; playerOneFirstCard])
        else
            if playerOneFirstCard > playerTwoFirstCard then
                playNextTurn (playerOneRest @ [playerOneFirstCard; playerTwoFirstCard]) playerTwoRest
            else
                playNextTurn playerOneRest (playerTwoRest @ [playerTwoFirstCard; playerOneFirstCard])


let PlayGameAndGetWinningScore input playGame =
    let [ playerOneDeck; playerTwoDeck ] = ParseInput input

    let winner = playGame playerOneDeck playerTwoDeck

    GetScore winner.WinningDeck

let SolvePuzzle (input: string) =
    PlayGameAndGetWinningScore input PlayGamePartOne

let SolvePuzzlePartTwo (input: string) =
    PlayGameAndGetWinningScore input (PlayGamePartTwo RoundMemory.empty)

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
