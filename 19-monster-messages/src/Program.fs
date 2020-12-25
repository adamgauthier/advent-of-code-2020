open System.IO
open System.Text.RegularExpressions

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

let MatchesRule ruleRegex message =
    Regex.IsMatch(message, "^" + ruleRegex + "$")

let rec RuleToRegex (rules: Map<RuleId, Rule>) ruleId =
    let rule = rules.[ruleId]
    match rule with
    | MatchesLiteral literal -> literal.ToString()
    | MatchesSubRules orSubRules ->
        let orSubRulesRegexes =
            orSubRules
            |> List.map (fun subRuleIds ->
                let subRulesRegexes = subRuleIds |> List.map (RuleToRegex rules)
                "(" + (String.concat ")(" subRulesRegexes) + ")"
            )
        "(" + (String.concat ")|(" orSubRulesRegexes) + ")"

let CountMessagesMatchingRule0 rules messages =
    let rule0Regex = RuleToRegex rules 0

    messages
    |> Seq.filter (MatchesRule rule0Regex)
    |> Seq.length

let SolvePuzzle input =
    let (rules, messages) = ParseInput input

    CountMessagesMatchingRule0 rules messages

let SolvePuzzlePartTwo input =
    let (rules, messages) = ParseInput input

    let newRules =
        rules
        |> Map.change 8 (fun _ ->
            let subRules = [
                for i in [1..6] do
                    List.replicate i 42
            ]
            Some (MatchesSubRules subRules)
        )
        |> Map.change 11 (fun _ ->
            let subRules = [
                for i in [1..6] do
                    List.replicate i 42 @ List.replicate i 31
            ]
            Some (MatchesSubRules subRules)
        )

    CountMessagesMatchingRule0 newRules messages

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
