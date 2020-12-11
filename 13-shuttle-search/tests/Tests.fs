module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns295`` () =
    let result =
        Program.SolvePuzzle("939
7,13,x,x,59,x,31,19".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(295L, result)
