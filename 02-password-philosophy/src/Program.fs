open System.IO

let IsPasswordValid (mustContain, min, max, password) =
    let characterCount =
        password
        |> Seq.filter (fun char -> char = mustContain)
        |> Seq.length

    characterCount >= min && characterCount <= max

let ParsePolicyAndPassword (policyAndPassword: string) =
    let [| policy; password |] =
        policyAndPassword.Split ':'
        |> Array.map (fun part -> part.Trim())

    let [| range ; character |] = policy.Split ' '

    let [| min ; max |] = range.Split '-' |> Array.map int

    (character.[0], min, max, password)

let SolvePuzzle policiesAndPasswords =
    policiesAndPasswords
    |> Seq.map ParsePolicyAndPassword
    |> Seq.filter IsPasswordValid
    |> Seq.length

[<EntryPoint>]
let main argv =

    let answer =
        File.ReadLines(argv.[0])
        |> SolvePuzzle

    printfn "Answer is %d" answer

    0
