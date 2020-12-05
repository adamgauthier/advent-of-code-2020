module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns514579`` () =
    let result = Program.SolvePuzzle [1721; 979; 366; 299; 675; 1456]
    Assert.Equal(514579, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenExample ThenReturns241861950`` () =
    let result = Program.SolvePuzzlePartTwo [1721; 979; 366; 299; 675; 1456]
    Assert.Equal(241861950, result)
