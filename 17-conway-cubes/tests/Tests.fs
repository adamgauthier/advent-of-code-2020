module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns165`` () =
    let result =
        Program.SolvePuzzle(".#.
..#
###".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(112, result)
