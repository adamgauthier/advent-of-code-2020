module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns14897079`` () =
    let result =
        Program.SolvePuzzle("5764801
17807724".Replace("\r\n", "\n").Split("\n") |> List.ofArray)

    Assert.Equal(14897079L, result)
