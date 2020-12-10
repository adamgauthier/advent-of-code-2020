module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns127`` () =
    let result =
        Program.SolvePuzzle 5 ("35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(127L, result)
