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


let IsInvalidValue fields ticketValue =
    fields |> (not << Seq.exists (fun field ->
        field.Ranges |> Seq.exists (fun range -> ticketValue >= range.Min && ticketValue <= range.Max)
    ))

let SolvePuzzle input =
    let (fields, _, nearbyTickets) = ParseInput input

    let allInvalidValues =
        nearbyTickets
        |> Seq.collect (fun nearbyTicket ->
            nearbyTicket.Values
            |> Seq.filter (IsInvalidValue fields)
        )

    allInvalidValues |> Seq.sum

[<EntryPoint>]
let main argv =

    let input =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> String.concat "\n"

    printfn "Answer for part one is %d" (SolvePuzzle input)

    0
