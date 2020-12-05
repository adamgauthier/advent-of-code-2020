module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns514579`` () =
    let result = Program.SolvePuzzle [1721; 979; 366; 299; 675; 1456]
    Assert.Equal(result, 514579)
