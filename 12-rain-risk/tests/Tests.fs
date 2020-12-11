module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns25`` () =
    let result =
        Program.SolvePuzzle("F10
N3
F7
R90
F11".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(25L, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenExample ThenReturns286`` () =
    let result =
        Program.SolvePuzzlePartTwo("F10
N3
F7
R90
F11".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(286L, result)
