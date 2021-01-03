module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExampleAfter10Moves ThenReturns92658374`` () =
    let result =
        Program.SolvePuzzle "389125467" 10u

    Assert.Equal("92658374", result)

[<Fact>]
let ``SolvePuzzle WhenExampleAfter100Moves ThenReturns67384529`` () =
    let result =
        Program.SolvePuzzle "389125467" 100u

    Assert.Equal("67384529", result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenExample ThenReturns149245887792`` () =
    let result =
        Program.SolvePuzzlePartTwo "389125467"

    Assert.Equal(149245887792L, result)
