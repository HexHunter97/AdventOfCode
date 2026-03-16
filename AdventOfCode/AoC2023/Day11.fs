namespace AoC2023

module Day11 =
    type private TileType =
        | TemporarySpace of bool * bool
        | Space of int64
        | Galaxy

    let private withCorrectSpaceSizes setter line =
        match line |> Array.contains Galaxy with
        | true ->
            line
            |> Array.map (fun value ->
                match value with
                | TemporarySpace(rowHasGalaxy, columnHasGalaxy) -> setter rowHasGalaxy columnHasGalaxy
                | _ -> value)
        | false -> line

    let private withFinalTileTypes spaceExpansionRate line =
        line
        |> Array.map (fun value ->
            match value with
            | TemporarySpace(rowHasGalaxy, columnHasGalaxy) when not (rowHasGalaxy && columnHasGalaxy) -> Space spaceExpansionRate
            | TemporarySpace(_, _) -> Space 1L
            | _ -> value)

    let private spaceAwareIndex tileAccessor line =
        line
        |> Array.tail
        |> Array.scan
            (fun (index, _) value ->
                match value |> tileAccessor with
                | Space size when size <> 1L ->
                    //asd
                    (index + size, value)
                | _ -> (index + 1L, value))
            (0L, line |> Array.head)

    let private parseSpace spaceExpansionRate (input: string) =
        input.Split(System.Environment.NewLine)
        |> Array.map (fun line ->
            line.ToCharArray()
            |> Array.map (fun char ->
                match char with
                | '#' -> Galaxy
                | _ -> TemporarySpace(false, false))
            |> withCorrectSpaceSizes (fun _ columnHasGalaxy -> TemporarySpace(true, columnHasGalaxy)))
        |> Array.transpose
        |> Array.map (fun line ->
            line
            |> withCorrectSpaceSizes (fun rowHasGalaxy _ -> TemporarySpace(rowHasGalaxy, true))
            |> withFinalTileTypes spaceExpansionRate)
        |> Array.transpose
        |> spaceAwareIndex Array.head
        |> Array.map (fun (coordY, line) ->
            line
            |> withFinalTileTypes spaceExpansionRate
            |> spaceAwareIndex id
            |> Seq.map (fun (coordX, tile) -> ({ X = coordX; Y = coordY }, tile)))
        |> Seq.concat
        |> Map.ofSeq

    let private CommonPart spaceExpansionRate input =
        input
        |> parseSpace spaceExpansionRate
        |> fun map -> map |> Map.filter (fun _ tile -> tile = Galaxy) |> Map.keys |> seq<Point>
        |> fun galaxyPositions -> Seq.allPairs galaxyPositions galaxyPositions
        |> Seq.map (fun (firstGalaxy, secondGalaxy) -> firstGalaxy - secondGalaxy |> Point.length)
        |> Seq.sum
        |> fun result -> result / 2L // both directions counted
        |> string

    let Solution1: Common.Solution = CommonPart 2L

    let Solution2: Common.Solution = CommonPart 1000000L
