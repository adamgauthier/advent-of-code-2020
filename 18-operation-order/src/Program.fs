open System.IO

type Token =
    | Addition
    | Multiplication
    | Literal of int64
    | OpenParentheses
    | CloseParentheses

type Expression = Token list

let ParseInput (input: string list): Expression list =
    input
    |> List.map (fun line ->
        let values = line.Replace("(", "( ").Replace(")", " )").Trim().Split(' ')

        values
        |> Array.map (fun char ->
            match char with
            | "+" -> Addition
            | "*" -> Multiplication
            | "(" -> OpenParentheses
            | ")" -> CloseParentheses
            | other -> Literal (int64 other)
        )
        |> Array.toList
    )

let EvaluateFirstAdditionOrMultiplication (expression: Expression) =
    let [Literal lit1; operator; Literal lit2] = expression |> List.take 3

    let result =
        match operator with
        | Addition -> lit1 + lit2
        | Multiplication -> lit1 * lit2

    [Literal result] @ (expression |> List.skip 3)

let EvaluateAdditionFirstThenMultiplication (expression: Expression) =
    match expression |> List.tryFindIndex ((=) Addition) with
    | Some additionIndex ->
        let [Literal lit1; Addition; Literal lit2] = expression.[additionIndex-1..additionIndex+1]

        let result = lit1 + lit2

        expression.[0..additionIndex-2] @ [Literal result] @ expression.[additionIndex+2..expression.Length-1]
    | None ->
        let [Literal lit1; Multiplication; Literal lit2] = expression |> List.take 3

        let result = lit1 * lit2

        [Literal result] @ (expression |> List.skip 3)

let rec Evaluate evaluateFirstOperationWithoutParantheses (expression: Expression) =
    match expression with
    | [Literal value] -> value
    | complex ->
        let evaluateNext = Evaluate evaluateFirstOperationWithoutParantheses
        match complex |> List.tryFindIndex ((=) OpenParentheses) with
        | Some startIndex ->
            let rec findClosingIndex currentIndex openCount =
                match complex |> List.item currentIndex with
                | CloseParentheses when openCount = 0 -> currentIndex
                | OpenParentheses -> findClosingIndex (currentIndex+1) (openCount+1)
                | CloseParentheses -> findClosingIndex (currentIndex+1) (openCount-1)
                | _ -> findClosingIndex (currentIndex+1) openCount

            let closingIndex = findClosingIndex (startIndex+1) 0

            let subResult = evaluateNext complex.[startIndex+1..closingIndex-1]

            evaluateNext (complex.[0..startIndex-1] @ [Literal subResult] @ complex.[closingIndex+1..complex.Length-1])
        | None ->
            let evaluated = evaluateFirstOperationWithoutParantheses complex

            evaluateNext evaluated


let SolvePuzzle input =
    let expressions = ParseInput input

    expressions
    |> Seq.sumBy (Evaluate EvaluateFirstAdditionOrMultiplication)

let SolvePuzzlePartTwo input =
    let expressions = ParseInput input

    expressions
    |> Seq.sumBy (Evaluate EvaluateAdditionFirstThenMultiplication)

[<EntryPoint>]
let main argv =

    let input =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer for part one is %d" (SolvePuzzle input)
    printfn "Answer for part two is %d" (SolvePuzzlePartTwo input)

    0
