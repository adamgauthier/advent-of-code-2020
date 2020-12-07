open System.IO

type HoldsBag = { BagColor: string; BagCount: int }
type Rule = { BagColor: string; HoldsBags: option<seq<HoldsBag>> }

let TakeFirstTwoWords (sentence: string) =
    sentence.Split(" ")
    |> Seq.take 2
    |> String.concat " "

let ParseRule (rule: string) =
    let [| beforeContain; afterContain |] = rule.Split(" contain ")

    let bagColor = TakeFirstTwoWords beforeContain
    let holds =
        match afterContain with
        | "no other bags." -> None
        | holdsBagsText ->
            let holdsBags = holdsBagsText.Split(", ")
            let holdsBagsParsed =
                holdsBags
                |> Seq.map (fun holdsBag ->
                    let [| countText; colorOne; colorTwo; _ |] = holdsBag.Split(" ")
                    let count = int countText
                    let color = [colorOne; colorTwo] |> String.concat " "

                    { BagColor=color; BagCount=count }
                )
            Some holdsBagsParsed

    { BagColor=bagColor; HoldsBags=holds }

let ContainsDirectly (bagColor: string) rule =
    match rule.HoldsBags with
    | Some subRules -> subRules |> Seq.exists (fun holdsBag -> holdsBag.BagColor = bagColor)
    | None -> false

let rec GetBagsThatEventuallyContain bagColor rules =
    seq {
        let bagsThatContainBagDirectly =
            rules
            |> Seq.filter (ContainsDirectly bagColor)
            |> Seq.map (fun rule -> rule.BagColor)

        yield! bagsThatContainBagDirectly

        let bagsThatContainBagIndirectly =
            bagsThatContainBagDirectly
            |> Seq.collect (fun bagColor -> rules |> GetBagsThatEventuallyContain bagColor)

        yield! bagsThatContainBagIndirectly
    }

let SolvePuzzle rules =
    rules
    |> Seq.map ParseRule
    |> GetBagsThatEventuallyContain "shiny gold"
    |> Seq.distinct
    |> Seq.length


let rec GetContainedBagCount (color: string) rules =
    let rule = rules |> Seq.find (fun rule -> rule.BagColor = color)
    match rule.HoldsBags with
    | Some holdsBags ->
        holdsBags
        |> Seq.sumBy (fun holdsBag -> holdsBag.BagCount + holdsBag.BagCount * (rules |> GetContainedBagCount holdsBag.BagColor))
    | None -> 0

let SolvePuzzlePartTwo rules =
    rules
    |> Seq.map ParseRule
    |> GetContainedBagCount "shiny gold"

[<EntryPoint>]
let main argv =

    let rules =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously

    printfn "Answer for part one is %d" (SolvePuzzle rules)
    printfn "Answer for part two is %d" (SolvePuzzlePartTwo rules)

    0
