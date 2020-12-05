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

    let [| aNumber ; bNumber |] = range.Split '-' |> Array.map int

    (character.[0], aNumber, bNumber, password)

let Solve policiesAndPasswords validator =
    policiesAndPasswords
    |> Seq.map ParsePolicyAndPassword
    |> Seq.filter validator
    |> Seq.length

let SolvePuzzle policiesAndPasswords =
    Solve policiesAndPasswords IsPasswordValid


let IsPasswordValidPartTwo (mustMatch, aNumber, bNumber, password: string) =
    let aNumberMatches = password.[aNumber - 1] = mustMatch
    let bNumberMatches = password.[bNumber - 1] = mustMatch

    aNumberMatches && not bNumberMatches || bNumberMatches && not aNumberMatches

let SolvePuzzlePartTwo policiesAndPasswords =
    Solve policiesAndPasswords IsPasswordValidPartTwo

[<EntryPoint>]
let main argv =

    let answer =
        File.ReadLines(argv.[0])
        |> match Array.tryItem 1 argv with
            | Some "2" ->  SolvePuzzlePartTwo
            | _ ->  SolvePuzzle

    printfn "Answer is %d" answer

    0
