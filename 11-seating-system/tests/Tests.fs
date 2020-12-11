module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns37`` () =
    let result =
        Program.SolvePuzzle("L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(37, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenExample ThenReturns26`` () =
    let result =
        Program.SolvePuzzlePartTwo("L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(26, result)
