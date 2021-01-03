open System.Collections.Generic

let ParseCupNumbers (input: string) =
    input
    |> Seq.map (fun char -> int (char.ToString()))
    |> Seq.toList

// using mutable data structure to dramatically improve peformance
type CupMap = Dictionary<int, int>

let rec PlayUntilMove (moveCountToSimulate: uint) (moveCount: uint) (currentCup: int) (cups: CupMap) =
    if moveCount = moveCountToSimulate then
        cups
    else
        let pickUp1 = cups.[currentCup]
        let pickUp2 = cups.[pickUp1]
        let pickUp3 = cups.[pickUp2]

        let rec findDestination toFind =
            if toFind = pickUp1 || toFind = pickUp2 || toFind = pickUp3 then
                findDestination (toFind - 1)
            else
                match cups.ContainsKey(toFind) with
                | true -> toFind
                | false -> findDestination (cups.Count)

        let destinationCup = findDestination (currentCup - 1)

        let nextToPickUp3 = cups.[pickUp3]
        let nextToDestinationCup = cups.[destinationCup]

        cups.[currentCup] <- nextToPickUp3
        cups.[destinationCup] <- pickUp1
        cups.[pickUp3] <- nextToDestinationCup

        PlayUntilMove moveCountToSimulate (moveCount + 1u) nextToPickUp3 cups

let rec CollectLabelsAfter (cups: CupMap) labels cup =
    match cups.[cup] with
    | nextCup when nextCup = 1 -> labels
    | nextCup -> CollectLabelsAfter cups (labels + nextCup.ToString()) nextCup

let BuildCupsMap sequence =
    let cups = new CupMap()
    sequence |> Seq.iter (fun (cup, nextCup) ->
        cups.[cup] <- nextCup
    )
    cups

let SolvePuzzle (input: string) (moveCountToSimulate: uint) =
    let numbers = ParseCupNumbers input

    let cupsSequence =
        numbers
        |> Seq.indexed
        |> Seq.map (fun (i, cup) ->
            let nextCup =
                if i < numbers.Length - 1 then
                    numbers.[i + 1]
                else
                    numbers.[0]

            (cup, nextCup)
        )

    let cups = BuildCupsMap cupsSequence

    let cupsAfterMoves = PlayUntilMove moveCountToSimulate 0u numbers.[0] cups

    CollectLabelsAfter cupsAfterMoves "" 1

let SolvePuzzlePartTwo (input: string) =
    let numbers = ParseCupNumbers input

    let cupsSequence =
        Seq.init 1000000 (fun i ->
            if (i + 1) < numbers.Length then
                (numbers.[i], numbers.[i + 1])
            elif (i + 1) = numbers.Length then
                (numbers.[i], i + 2)
            elif (i + 1) = 1000000 then
                (i + 1, numbers.[0])
            else
                (i + 1, i + 2)
        )

    let cups = BuildCupsMap cupsSequence

    let cupsAfterMoves = PlayUntilMove 10000000u 0u numbers.[0] cups

    let nextTo1 = cupsAfterMoves.[1]
    let nextToNextTo1 = cupsAfterMoves.[nextTo1]

    (Checked.(*)) (int64 nextTo1) (int64 nextToNextTo1)

[<EntryPoint>]
let main argv =

    printfn "Answer for part one is %s" (SolvePuzzle "167248359" 100u)
    printfn "Answer for part two is %d" (SolvePuzzlePartTwo "167248359")

    0
