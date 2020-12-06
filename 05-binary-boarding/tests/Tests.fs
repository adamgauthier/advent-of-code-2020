module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns820`` () =
    let result = Program.SolvePuzzle ["BFFFBBFRRR"; "FFFBBBFRRR"; "BBFFBBFRLL"]

    Assert.Equal(820, result)
