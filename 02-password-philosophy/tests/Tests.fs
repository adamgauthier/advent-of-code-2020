module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns2`` () =
    let result = Program.SolvePuzzle ["1-3 a: abcde"; "1-3 b: cdefg"; "2-9 c: ccccccccc"]
    Assert.Equal(2, result)
