module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns165`` () =
    let result =
        Program.SolvePuzzle("mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(165L, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenExample ThenReturns208`` () =
    let result =
        Program.SolvePuzzlePartTwo("mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(208UL, result)
