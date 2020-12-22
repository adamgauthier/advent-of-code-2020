module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenFirstExample ThenReturns436`` () =
    let result =
        Program.SolvePuzzle("0,3,6")

    Assert.Equal(436, result)

[<Fact>]
let ``SolvePuzzle WhenSecondExample ThenReturns1`` () =
    let result =
        Program.SolvePuzzle("1,3,2")

    Assert.Equal(1, result)

[<Fact>]
let ``SolvePuzzle WhenThirdExample ThenReturns10`` () =
    let result =
        Program.SolvePuzzle("2,1,3")

    Assert.Equal(10, result)

[<Fact>]
let ``SolvePuzzle WhenFourthExample ThenReturns27`` () =
    let result =
        Program.SolvePuzzle("1,2,3")

    Assert.Equal(27, result)

[<Fact>]
let ``SolvePuzzle WhenFifthExample ThenReturns78`` () =
    let result =
        Program.SolvePuzzle("2,3,1")

    Assert.Equal(78, result)

[<Fact>]
let ``SolvePuzzle WhenSixthExample ThenReturns438`` () =
    let result =
        Program.SolvePuzzle("3,2,1")

    Assert.Equal(438, result)

[<Fact>]
let ``SolvePuzzle WhenSeventhExample ThenReturns1836`` () =
    let result =
        Program.SolvePuzzle("3,1,2")

    Assert.Equal(1836, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenFirstExample ThenReturns175594`` () =
    let result =
        Program.SolvePuzzlePartTwo("0,3,6")

    Assert.Equal(175594, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenSecondExample ThenReturns2578`` () =
    let result =
        Program.SolvePuzzlePartTwo("1,3,2")

    Assert.Equal(2578, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenThirdExample ThenReturns3544142`` () =
    let result =
        Program.SolvePuzzlePartTwo("2,1,3")

    Assert.Equal(3544142, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenFourthExample ThenReturns261214`` () =
    let result =
        Program.SolvePuzzlePartTwo("1,2,3")

    Assert.Equal(261214, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenFifthExample ThenReturns6895259`` () =
    let result =
        Program.SolvePuzzlePartTwo("2,3,1")

    Assert.Equal(6895259, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenSixthExample ThenReturns18`` () =
    let result =
        Program.SolvePuzzlePartTwo("3,2,1")

    Assert.Equal(18, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenSeventhExample ThenReturns362`` () =
    let result =
        Program.SolvePuzzlePartTwo("3,1,2")

    Assert.Equal(362, result)
