module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns5`` () =
    let result =
        Program.SolvePuzzle ("nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(5, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenExample ThenReturns8`` () =
    let result =
        Program.SolvePuzzlePartTwo ("nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(8, result)
