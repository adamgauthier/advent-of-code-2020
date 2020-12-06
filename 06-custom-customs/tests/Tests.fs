module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns11`` () =
    let result =
        Program.SolvePuzzle "abc

a
b
c

ab
ac

a
a
a
a

b"
    Assert.Equal(11, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenExample ThenReturns6`` () =
    let result =
        Program.SolvePuzzlePartTwo "abc

a
b
c

ab
ac

a
a
a
a

b"
    Assert.Equal(6, result)
