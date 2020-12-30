module Tests

open Xunit

[<Fact>]
let ``SolvePuzzle WhenExample ThenReturns5`` () =
    let result =
        Program.SolvePuzzle("mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)".Replace("\r\n", "\n").Split("\n") |> List.ofArray)

    Assert.Equal(5, result)

[<Fact>]
let ``SolvePuzzlePartTwo WhenExample ThenReturnsExpectedIngredients`` () =
    let result =
        Program.SolvePuzzlePartTwo("mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)".Replace("\r\n", "\n").Split("\n") |> List.ofArray)

    Assert.Equal("mxmxvkd,sqjhc,fvjkl", result)
