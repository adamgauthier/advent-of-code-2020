module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns306`` () =
    let result =
        Program.SolvePuzzle("Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10".Replace("\r\n", "\n"))

    Assert.Equal(306, result)
