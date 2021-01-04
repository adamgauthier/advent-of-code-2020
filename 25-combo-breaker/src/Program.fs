open System.IO

let GetRemainder value subjectNumber =
    let multiplied = Checked.(*) value subjectNumber
    multiplied % 20201227L

let Transform subjectNumber loopSize =
    let rec loop i value =
        if i = loopSize then
            value
        else
            let remainder = GetRemainder value subjectNumber
            loop (i+1) remainder

    loop 0 1L

let FindLoopSize cardPublicKey doorPublicKey =
    let subjectNumber = 7L

    let rec loop i value =
        if value = cardPublicKey then
            (cardPublicKey, i)
        elif value = doorPublicKey then
            (doorPublicKey, i)
        else
            let remainder = GetRemainder value subjectNumber
            loop (i+1) remainder

    loop 0 1L


let SolvePuzzle input =
    let [ cardPublicKey; doorPublicKey ] = input |> List.map int64

    let (devicePublicKey, loopSize) = FindLoopSize cardPublicKey doorPublicKey

    let otherDeviceKey = if devicePublicKey = cardPublicKey then doorPublicKey else cardPublicKey

    let encryptionKey = Transform otherDeviceKey loopSize

    encryptionKey

[<EntryPoint>]
let main argv =

    let input =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer is %d" (SolvePuzzle input)

    0
