namespace AoC2023


module Day03 =
    type private SchemaValue =
        | Empty
        | Part of IsGear: bool
        | PartNumberDigit of char
        | IdentifiedPartNumber of Value: int * FirstDigitPosition: Point

    let private getAdjacentFields = getOffsetFields adjacentFieldOffsets

    let private getFullPartNumberAndDigitStart (schema: Map<Point, SchemaValue>) (point: Point, partNumberFragment: char) =
        let rec takeWhileNumber offset point schema : list<Point * char> =
            offsetPoint point offset
            |> fun targetPoint ->
                Map.tryFind targetPoint schema
                |> fun valueOpt ->
                    match valueOpt with
                    | Some(PartNumberDigit partNum) -> (targetPoint, partNum) :: (takeWhileNumber offset targetPoint schema)
                    | _ -> List.empty

        (takeWhileNumber (-1, 0) point schema)
        @ (takeWhileNumber (1, 0) point schema)
        @ [ (point, partNumberFragment) ]
        |> Seq.sortBy (fun (point, _) -> point.X)
        |> Seq.fold (fun (start: Point option, currentFragment) (point, value) -> (Some(Option.defaultValue point start), currentFragment + value.ToString())) (None, "")
        |> fun (start, partNumber) -> IdentifiedPartNumber(partNumber |> int, start.Value)

    let private calculatePartNumbers (schema: Map<Point, SchemaValue>) =
        schema
        |> Map.map (fun point value ->
            match value with
            | PartNumberDigit partNumber -> getFullPartNumberAndDigitStart schema (point, partNumber)
            | _ -> value)

    let private applyToPartNumbersAdjacentToPartsAndSumResults (perPartPartNumberOperation: seq<SchemaValue> -> int) (shouldPerformOnPart: bool -> bool) (schema: Map<Point, SchemaValue>) =
        schema
        |> Map.map (fun point value ->
            match value with
            | Part isGear when shouldPerformOnPart (isGear) ->
                getAdjacentFields schema point
                |> Seq.filter (fun (_, value) ->
                    match value with
                    | IdentifiedPartNumber(_, _) -> true
                    | _ -> false)
                |> Seq.groupBy (fun (_, value) ->
                    match value with
                    | IdentifiedPartNumber(_, firstDigitPosition) -> Some(firstDigitPosition)
                    | _ -> None)
                |> Seq.map (fun (_, values) -> Seq.head values |> fun (_, value) -> value)
                |> perPartPartNumberOperation
            | _ -> 0)
        |> Map.toSeq
        |> Seq.sumBy (fun (_, value) -> value)

    let private parseSchema (input: string) =
        input.Split(System.Environment.NewLine)
        |> Seq.indexed
        |> Seq.map (fun (indexY, line) ->
            line.ToCharArray()
            |> Seq.indexed
            |> Seq.map (fun (indexX, char) ->
                ({ X = indexX; Y = indexY },
                 match char with
                 | '.' -> Empty
                 | _ when System.Char.IsDigit(char) -> PartNumberDigit(char)
                 | '*' -> Part(true)
                 | _ -> Part(false))))
        |> Seq.collect id
        |> Map.ofSeq
        |> calculatePartNumbers

    let private CommonPart =
        fun perPartPartNumberOperation shouldPerformOnPart input ->
            input
            |> parseSchema
            |> applyToPartNumbersAdjacentToPartsAndSumResults perPartPartNumberOperation shouldPerformOnPart
            |> string

    let Solution1: Common.Solution =
        CommonPart
            (fun partNumbers ->
                partNumbers
                |> Seq.sumBy (fun partNumber ->
                    match partNumber with
                    | IdentifiedPartNumber(value, _) -> value
                    | _ -> 0))
            (fun _ -> true)

    let Solution2: Common.Solution =
        CommonPart
            (fun partNumbers ->
                match (Seq.length partNumbers) with
                | 2 ->
                    partNumbers
                    |> Seq.fold
                        (fun accummulator partNumber ->
                            match partNumber with
                            | IdentifiedPartNumber(value, _) -> value * accummulator
                            | _ -> accummulator)
                        1
                | _ -> 0)
            (fun isGear -> isGear)
