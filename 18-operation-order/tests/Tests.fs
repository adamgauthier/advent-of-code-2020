module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenFirstExample ThenReturns71`` () =
    let result =
        Program.SolvePuzzle("1 + 2 * 3 + 4 * 5 + 6".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(71L, result)

[<Fact>]
let ``SolvePuzzle WhenSecondExample ThenReturns26`` () =
    let result =
        Program.SolvePuzzle("2 * 3 + (4 * 5)".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(26L, result)

[<Fact>]
let ``SolvePuzzle WhenThirdExample ThenReturns437`` () =
    let result =
        Program.SolvePuzzle("5 + (8 * 3 + 9 + 3 * 4 * 3)".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(437L, result)

[<Fact>]
let ``SolvePuzzle WhenFourthExample ThenReturns12240`` () =
    let result =
        Program.SolvePuzzle("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(12240L, result)

[<Fact>]
let ``SolvePuzzle WhenFifthExample ThenReturns13632`` () =
    let result =
        Program.SolvePuzzle("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(13632L, result)
