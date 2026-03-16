namespace AoC2023

module Day09 =
    let private calculateNextValue preProcess topLevel =
        let rec calculateNextDifference topLevelReverse =
            topLevelReverse
            |> List.tail
            |> List.fold (fun (previous, lowerLevel) current -> (current, (previous - current) :: lowerLevel)) (topLevelReverse |> List.head, [])
            |> (fun (_, lowerLevel) ->
                match lowerLevel with
                | x when x |> List.exists ((<>) 0L) ->
                    (calculateNextDifference (lowerLevel |> List.rev))
                    + (topLevelReverse |> List.head)
                | _ -> topLevelReverse |> List.head)

        topLevel |> preProcess |> Seq.toList |> calculateNextDifference

    let private CommonPart preProcess (input: string) =
        input.Split(System.Environment.NewLine)
        |> Seq.map (fun x -> x.Split(" ") |> Array.map int64 |> calculateNextValue preProcess)
        |> Seq.sum
        |> string

    let Solution1: Common.Solution = CommonPart Seq.rev

    let Solution2: Common.Solution = CommonPart id
