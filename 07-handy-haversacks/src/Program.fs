open System.IO

type Rule = { BagColor:string; ContainsBags:string }

let TakeFirstTwoWords (sentence: string) =
    sentence.Split(" ")
    |> Seq.take 2
    |> String.concat " "

let ParseRule (rule: string) =
    let [| beforeContain; afterContain |] = rule.Split(" contain ")

    let bagColor = TakeFirstTwoWords beforeContain

    { BagColor=bagColor; ContainsBags=afterContain }

let ContainsDirectly (bagColor: string) rule =
    rule.ContainsBags.Contains(bagColor)

let rec GetBagsThatEventuallyContain bagColor rules =
    seq {
        let rulesThatContainBagDirectly = rules |> Seq.filter (ContainsDirectly bagColor)
        yield! rulesThatContainBagDirectly

        let containsIndirectly =
            rulesThatContainBagDirectly
            |> Seq.collect (fun rule -> rules |> GetBagsThatEventuallyContain rule.BagColor)

        yield! containsIndirectly
    }

let SolvePuzzle rules =
    rules
    |> Seq.map ParseRule
    |> GetBagsThatEventuallyContain "shiny gold"
    |> Seq.distinct
    |> Seq.length

[<EntryPoint>]
let main argv =

    let rules =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously

    printfn "Answer for part one is %d" (SolvePuzzle rules)

    0
