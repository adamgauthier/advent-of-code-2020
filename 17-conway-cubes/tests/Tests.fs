module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns112`` () =
    let result =
        Program.SolvePuzzle(".#.
..#
###".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(112, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenExample ThenReturns848`` () =
    let result =
        Program.SolvePuzzlePartTwo(".#.
..#
###".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(848, result)
