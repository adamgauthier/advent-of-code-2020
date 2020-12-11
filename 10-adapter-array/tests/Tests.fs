module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenFirstExample ThenReturns7And5`` () =
    let result =
        Program.SolvePuzzle ("16
10
15
5
1
11
7
19
6
12
4".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(7 * 5, result)

[<Fact>]
let ``SolvePuzzle WhenSecondExample ThenReturns22And10`` () =
    let result =
        Program.SolvePuzzle ("28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3".Replace("\r\n", "\n").Split("\n") |> List.ofSeq)

    Assert.Equal(22 * 10, result)
