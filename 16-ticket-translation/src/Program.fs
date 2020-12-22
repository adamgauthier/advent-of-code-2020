open System.IO

type Range = { Min: int64; Max: int64 }
type Field = { Name: string; Ranges: Range list }
type Ticket = { Values: int64 list }

let ParseInput (input: string) =
    let [| rawFields; rawMyTicket; rawNearbyTickets |] = input.Split("\n\n")

    let fields =
        rawFields.Split("\n")
        |> Array.map (fun f ->
            let [| fieldName; rules |] = f.Split(": ")

            let ranges =
                rules.Split(" or ")
                |> Array.map (fun r ->
                    let [| min; max |] = r.Split("-") |> Array.map int64
                    { Min=min; Max=max }
                )
                |> Array.toList

            { Name=fieldName; Ranges=ranges }
        )
        |> Array.toList

    let parseTicket (raw: string) =
        let parsedNumbers =
            raw.Split(",")
            |> Array.map int64
            |> Array.toList
        { Values=parsedNumbers }

    let myTicket =
        rawMyTicket.Split("\n")
        |> Array.skip 1
        |> Array.map parseTicket
        |> Array.exactlyOne

    let nearbyTickets =
        rawNearbyTickets.Split("\n")
        |> Array.skip 1
        |> Array.map parseTicket
        |> Array.toList

    (fields, myTicket, nearbyTickets)

let IsValidValueForField ticketValue field =
    field.Ranges |> Seq.exists (fun range -> ticketValue >= range.Min && ticketValue <= range.Max)

let IsValidValue fields ticketValue =
    fields
    |> Seq.exists (IsValidValueForField ticketValue)

let SolvePuzzle input =
    let (fields, _, nearbyTickets) = ParseInput input

    let allInvalidValues =
        nearbyTickets
        |> Seq.collect (fun nearbyTicket ->
            nearbyTicket.Values
            |> Seq.filter (not << IsValidValue fields)
        )

    allInvalidValues |> Seq.sum


let rec FindSolution (indexedFieldsWithoutSolution: list<int * list<Field>>) currentSolution =
    if List.isEmpty indexedFieldsWithoutSolution then
        currentSolution
    else
        let (indexesWithOneField, indexesWithMultipleFields) =
            indexedFieldsWithoutSolution
            |> List.partition (fun (_, possibleFields) -> List.length possibleFields = 1)

        let indexesWithSolution = indexesWithOneField |> List.map (fun (index, fields) -> (index, fields |> List.exactlyOne))

        let newIndexedFieldsLeft =
            indexesWithMultipleFields
            |> List.map (fun (index, fields) ->
                let fieldsWithSolution = indexesWithSolution |> List.map snd
                let newPossibleFields = fields |> List.except fieldsWithSolution

                (index, newPossibleFields)
            )

        let newSolution = currentSolution @ indexesWithSolution

        FindSolution newIndexedFieldsLeft newSolution


let SolvePuzzlePartTwo input =
    let (fields, myTicket, nearbyTickets) = ParseInput input

    let validTickets = nearbyTickets |> List.filter (fun nearbyTicket ->
        nearbyTicket.Values |> Seq.forall (IsValidValue fields)
    )

    let indexedPossibleFields =
        [0..myTicket.Values.Length-1]
        |> List.map (fun valueIndex ->
            let ticketValuesForIndex =
                validTickets
                |> List.map (fun ticket -> ticket.Values.[valueIndex])

            let possibleValidFields =
                fields |> List.filter (fun field ->
                    ticketValuesForIndex
                    |> Seq.forall (fun value -> IsValidValueForField value field)
                )

            (valueIndex, possibleValidFields)
        )

    let solution = FindSolution indexedPossibleFields []

    let departureFieldsMultiplied =
        solution
        |> List.filter (fun (_, field) -> field.Name.StartsWith("departure "))
        |> List.map (fun (i, _) -> myTicket.Values.[i])
        |> List.fold Checked.(*) 1L

    departureFieldsMultiplied

[<EntryPoint>]
let main argv =

    let input =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> String.concat "\n"

    printfn "Answer for part one is %d" (SolvePuzzle input)
    printfn "Answer for part two is %d" (SolvePuzzlePartTwo input)

    0
