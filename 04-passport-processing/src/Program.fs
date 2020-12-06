open System.IO

let CleanInput (input: string) =
    input.Replace("\r\n", "\n")

let HasAllRequiredFields (passport: Map<string, string>) =
    ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    |> Seq.forall passport.ContainsKey

let ParsePassport (text: string) =
    text.Replace("\n", " ").Split(' ')
    |> Seq.map (fun field ->
        let [| name; value |] = field.Split(':')
        (name, value)
    )
    |> Map.ofSeq

let SolvePuzzle (input: string) =
    let cleaned = CleanInput input

    let passports =
        cleaned.Split("\n\n")
        |> Seq.map ParsePassport

    passports
    |> Seq.filter HasAllRequiredFields
    |> Seq.length

[<EntryPoint>]
let main argv =

    let input =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> String.concat "\n"

    printfn "Answer for part one is %d" (SolvePuzzle input)

    0
