let GetItemNormalizedIndex<'T> index (list: list<'T>) =
    list.[index % list.Length]

let rec PlayUntilMove (moveCountToSimulate: uint) (moveCount: uint) (currentCupIndex: int) (cups: int list) =
    if moveCount = moveCountToSimulate then
        cups
    else
        let currentCup = cups |> GetItemNormalizedIndex currentCupIndex

        let cup1 = cups |> GetItemNormalizedIndex (currentCupIndex + 1)
        let cup2 = cups |> GetItemNormalizedIndex (currentCupIndex + 2)
        let cup3 = cups |> GetItemNormalizedIndex (currentCupIndex + 3)

        let pickedUpCups = [cup1; cup2; cup3]

        let cupsWithoutPickedUp = cups |> List.except pickedUpCups

        let rec findDestination toFind =
            match cups |> List.tryFind ((=) toFind) with
            | Some value when pickedUpCups |> List.contains value -> findDestination (toFind - 1)
            | Some otherValue -> otherValue
            | None -> cupsWithoutPickedUp |> List.max

        let destinationCup = findDestination (currentCup - 1)
        let destinationCupIndex = cupsWithoutPickedUp |> List.findIndex ((=) destinationCup)

        let newCups = cupsWithoutPickedUp.[0..destinationCupIndex] @ pickedUpCups @ cupsWithoutPickedUp.[destinationCupIndex+1..cups.Length-1]
        let newCurrentCupIndex = (newCups |> List.findIndex ((=) currentCup)) + 1

        PlayUntilMove moveCountToSimulate (moveCount + 1u) newCurrentCupIndex newCups

let rec CollectLabelsAfter1 cups labels currentIndex =
    match cups |> GetItemNormalizedIndex currentIndex with
    | cup when cup = 1 -> labels
    | cup -> CollectLabelsAfter1 cups (labels + cup.ToString()) (currentIndex + 1)

let SolvePuzzle (input: string) (moveCountToSimulate: uint) =
    let numbers =
        input
        |> Seq.map (fun char -> int (char.ToString()))
        |> Seq.toList

    let cupsAfterMoveCount = PlayUntilMove moveCountToSimulate 0u 0 numbers
    let indexCup1 = cupsAfterMoveCount |> List.findIndex ((=) 1)

    CollectLabelsAfter1 cupsAfterMoveCount "" (indexCup1 + 1)

[<EntryPoint>]
let main argv =

    printfn "Answer for part one is %s" (SolvePuzzle "167248359" 100u)

    0
