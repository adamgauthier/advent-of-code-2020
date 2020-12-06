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

[<Fact>]
let ``SolvePuzzlePartTwo WhenExample ThenReturns336`` () =
    let result = Program.SolvePuzzlePartTwo [
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
    Assert.Equal(336UL, result)
