open System.IO

type RuleId = int

type Rule =
    | MatchesLiteral of char
    | MatchesSubRules of RuleId list list

let ParseInput (input: string): Map<RuleId, Rule> * string list =
    let [| rawRules; rawReceivedMessages |] = input.Split("\n\n")

    let parsedRules =
        rawRules.Split("\n")
        |> Seq.map (fun rawRule ->
            let [| ruleId; ruleContent |] = rawRule.Split(": ")

            let parsedRule =
                match ruleContent.StartsWith('"') with
                | true -> MatchesLiteral (ruleContent.[1])
                | false ->
                    let subRules =
                        ruleContent.Split(" | ")
                        |> Array.map (fun subRule ->
                            subRule.Split(" ")
                            |> Array.map int
                            |> Array.toList
                        )
                        |> Array.toList
                    MatchesSubRules subRules

            (int ruleId, parsedRule)
        )
        |> Map.ofSeq

    (parsedRules, rawReceivedMessages.Split("\n") |> Array.toList)

let rec CountRuleLength rule allRules =
    match rule with
    | MatchesLiteral _ -> 1
    | MatchesSubRules subRules ->
        subRules.[0]
        |> List.map (fun ruleId -> allRules |> Map.find ruleId)
        |> List.sumBy (fun rule -> CountRuleLength rule allRules)

let rec MatchesRule rule allRules message =
    match rule with
    | MatchesLiteral literal -> message = literal.ToString()
    | MatchesSubRules [ subRuleIds ] ->
        let (matches, messageLeft) =
            subRuleIds
            |> Seq.fold (fun (matches, currentMessage: string) ruleId ->
                let subRule = allRules |> Map.find ruleId
                let length = allRules |> CountRuleLength subRule

                let newCurrentMessage = currentMessage.[length..currentMessage.Length-1]
                let newMatches = matches && MatchesRule subRule allRules currentMessage.[0..length-1]

                (newMatches, newCurrentMessage)
            ) (true, message)

        matches && messageLeft.Length = 0
    | MatchesSubRules orSubRuleIds ->
        orSubRuleIds
        |> Seq.exists (fun subRuleIds ->
            MatchesRule (MatchesSubRules [subRuleIds]) allRules message
        )

let SolvePuzzle input =
    let (rules, messages) = ParseInput input

    messages
    |> Seq.filter (MatchesRule rules.[0] rules)
    |> Seq.length

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
