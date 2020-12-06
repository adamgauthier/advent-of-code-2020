module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns7`` () =
    let result = Program.SolvePuzzle [
        "..##.......";
        "#...#...#..";
        ".#....#..#.";
        "..#.#...#.#";
        ".#...##..#.";
        "..#.##.....";
        ".#.#.#....#";
        ".#........#";
        "#.##...#...";
        "#...##....#";
        ".#..#...#.#"
    ]
    Assert.Equal(7, result)
