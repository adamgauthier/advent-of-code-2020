open System.IO
open System.Text.RegularExpressions
open System

let CleanInput (input: string) =
    input.Replace("\r\n", "\n")

let ParsePassport (text: string) =
    let fields = text.Replace("\n", " ").Split(' ')

    fields
    |> Seq.map (fun field ->
        let [| name; value |] = field.Split(':')
        (name, value)
    )
    |> Map.ofSeq

let CountValidPassports isValid (input: string) =
    let cleaned = CleanInput input

    let passports =
        cleaned.Split("\n\n")
        |> Seq.map ParsePassport

    passports
    |> Seq.filter isValid
    |> Seq.length

let HasAllRequiredFields (passport: Map<string, string>) =
    ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    |> Seq.forall passport.ContainsKey

let SolvePuzzle (input: string) =
    input |> (CountValidPassports HasAllRequiredFields)

let SolvePuzzlePartTwo (input: string) =
    let fieldExistsAndMatches name predicate (passport: Map<string, string>) =
        match passport |> Map.tryFind name with
        | Some value -> predicate(value)
        | _ -> false

    let isValidYear name predicate passport =
        passport |> fieldExistsAndMatches name (fun value ->
            match UInt16.TryParse(value) with
            | true, parsed -> predicate(parsed)
            | _ -> false
        )

    let isBirthYearValid passport =
        passport |> isValidYear "byr" (fun value -> value >= 1920us && value <= 2002us)

    let isIssueYearValid passport =
        passport |> isValidYear "iyr" (fun value -> value >= 2010us && value <= 2020us)

    let isExpirationYearValid passport =
        passport |> isValidYear "eyr" (fun value -> value >= 2020us && value <= 2030us)

    let isHeightValid passport =
        passport |> fieldExistsAndMatches "hgt" (fun value ->
            if value.Length = 5 && value.EndsWith("cm") then
                match UInt16.TryParse(value.Substring(0, 3)) with
                | true, parsed -> parsed >= 150us && parsed <= 193us
                | _ -> false
            elif value.Length = 4 && value.EndsWith("in") then
                match UInt16.TryParse(value.Substring(0, 2)) with
                | true, parsed -> parsed >= 59us && parsed <= 76us
                | _ -> false
            else false
        )

    let isHairColorValid passport =
        passport |> fieldExistsAndMatches "hcl" (fun value -> Regex.IsMatch(value, @"^#(\d|[a-f]){6}$"))

    let isEyeColorValid passport =
        passport |> fieldExistsAndMatches "ecl" (fun value ->
            match value with
            | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
            | _ -> false
        )

    let isPassportIdValid passport =
        passport |> fieldExistsAndMatches "pid" (fun value -> Regex.IsMatch(value, @"^\d{9}$"))

    let isPassportValid passport =
        [isBirthYearValid; isIssueYearValid; isExpirationYearValid; isHeightValid; isHairColorValid; isEyeColorValid; isPassportIdValid]
        |> Seq.forall (fun isValid -> isValid(passport))

    input |> (CountValidPassports isPassportValid)

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
