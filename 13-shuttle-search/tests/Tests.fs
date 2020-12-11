module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns295`` () =
    let result =
        Program.SolvePuzzle("939
7,13,x,x,59,x,31,19".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(295L, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenFirstExample ThenReturns1068781`` () =
    let result =
        Program.SolvePuzzlePartTwo("939
7,13,x,x,59,x,31,19".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(1068781L, result)
