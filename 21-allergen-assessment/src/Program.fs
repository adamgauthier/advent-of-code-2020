open System.IO

type Food = { Ingredients: Set<string>; Allergens: Set<string> }

let ParseFood (input: string): Food =
    let [| rawIngredients; rawAllergen |] = input.Split(" (")

    let ingredients = rawIngredients.Split(" ") |> Set.ofArray
    let allergens = rawAllergen.TrimEnd(')').Substring(9).Split(", ") |> Set.ofArray

    { Ingredients = ingredients; Allergens = allergens }

type AllergenMatchingIngredients = { Allergen: string; PossibleIngredients: Set<string> }
type AllergenMatchingIngredient = { Allergen: string; Ingredient: string }

let rec MatchAllergensWithIngredients (foods: Food list) allergensWithoutMatch allergensWithMatch =
    if List.isEmpty allergensWithoutMatch then
        allergensWithMatch
    else
        let allergensMatchingIngredient =
            allergensWithoutMatch
            |> List.map (fun allergen ->
                let containingFoods =
                    foods
                    |> List.filter (fun food -> food.Allergens |> Set.contains allergen)

                let ingredientsInContainingFoods =
                    containingFoods
                    |> List.map (fun food -> food.Ingredients)

                { Allergen = allergen; PossibleIngredients = Set.intersectMany ingredientsInContainingFoods }
            )
            |> List.choose (fun matchingIngredients ->
                match matchingIngredients.PossibleIngredients |> Seq.tryExactlyOne with
                | Some onlyOneIngredientMatch ->
                    Some { Allergen = matchingIngredients.Allergen; Ingredient = onlyOneIngredientMatch }
                | None -> None
            )

        let allergensWithNewMatch = allergensMatchingIngredient |> List.map (fun a -> a.Allergen)
        let ingredientsWithNewMatch = allergensMatchingIngredient |> List.map (fun a -> a.Ingredient)

        let newAllergensWithoutMatch = allergensWithoutMatch |> List.except allergensWithNewMatch
        let newAllergensWithMatch = allergensWithMatch |> List.append allergensMatchingIngredient

        let newFoods = foods |> List.map (fun food ->
            let ingredientsWithoutMatch = food.Ingredients |> Seq.except ingredientsWithNewMatch |> Set.ofSeq
            let foodAllergensWithoutMatch = food.Allergens |> Seq.except allergensWithNewMatch |> Set.ofSeq

            { Ingredients = ingredientsWithoutMatch; Allergens = foodAllergensWithoutMatch }
        )

        MatchAllergensWithIngredients newFoods newAllergensWithoutMatch newAllergensWithMatch


let MatchAllAllergensWithIngredients foods =
    let allAllergens = foods |> List.collect (fun food -> food.Allergens |> Set.toList) |> List.distinct

    MatchAllergensWithIngredients foods allAllergens []


let SolvePuzzle input =
    let foods = input |> List.map ParseFood

    let matchedAllergens = MatchAllAllergensWithIngredients foods

    let ingredientsMatchingAnAllergen = matchedAllergens |> List.map (fun matched -> matched.Ingredient)

    let timesIngredientsWithoutAllergenAppear =
        foods |> List.sumBy (fun food ->
            food.Ingredients
            |> Set.filter (fun ingredient -> ingredientsMatchingAnAllergen |> (not << Seq.contains ingredient))
            |> Seq.length
        )

    timesIngredientsWithoutAllergenAppear

[<EntryPoint>]
let main argv =

    let input =
        File.ReadAllLinesAsync(argv.[0])
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> List.ofArray

    printfn "Answer for part one is %d" (SolvePuzzle input)

    0
