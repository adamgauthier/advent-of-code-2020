module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns127`` () =
    let result =
        Program.SolvePuzzle ("F10
N3
F7
R90
F11".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(25L, result)
