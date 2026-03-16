namespace AoC2023

module Day05 =
    type MappingType =
        | Seed
        | Soil
        | Fertilizer
        | Water
        | Light
        | Temperature
        | Humidity
        | Location

    type SeedRange = { Start: int64; End: int64 }

    type MapLine =
        { DestinationStart: int64
          SourceStart: int64
          SourceEnd: int64 }

    type ConversionMap =
        { Source: MappingType
          Destination: MappingType
          Lines: MapLine array }

    type Chunk =
        | Seeds of seq<SeedRange>
        | MapChunk of ConversionMap

    let convertChunkToMap sourceType destinationType (chunk: string) =
        MapChunk(
            { Source = sourceType
              Destination = destinationType
              Lines =
                chunk.Split(System.Environment.NewLine)
                |> Seq.tail
                |> Seq.map (fun x ->
                    x.Split(' ')
                    |> Seq.map int64
                    |> fun line ->
                        { DestinationStart = line |> Seq.item 0
                          SourceStart = line |> Seq.item 1
                          SourceEnd = (line |> Seq.item 2) + (line |> Seq.item 1) - 1L })
                |> Seq.toArray }
        )

    let rec orderMaps maps nextType orderedMap =
        match maps |> Seq.tryFind (fun map -> map.Source = nextType) with
        | Some(value) -> orderedMap @ [ value ] |> orderMaps maps value.Destination
        | None -> orderedMap

    let applyLineToSeedRange line seedRange =
        { Start = seedRange.Start + (line.DestinationStart - line.SourceStart)
          End = seedRange.End + (line.DestinationStart - line.SourceStart) }

    let applyMapLine mappingSeedRanges mapLine =
        mappingSeedRanges
        |> Seq.map (fun (mappingSeedRange, isMapped) ->
            match mapLine with
            // already mapped
            | _ when isMapped -> [ (mappingSeedRange, true) ]
            | _ when // whole range matched
                mapLine.SourceStart <= mappingSeedRange.Start
                && mapLine.SourceEnd >= mappingSeedRange.End
                ->
                [ (applyLineToSeedRange mapLine mappingSeedRange, true) ]
            | _ when // upper part matched
                mapLine.SourceStart > mappingSeedRange.Start
                && mapLine.SourceStart <= mappingSeedRange.End
                && mapLine.SourceEnd >= mappingSeedRange.End
                ->
                [ ({ mappingSeedRange with
                      End = mapLine.SourceStart - 1L },
                   false)
                  (applyLineToSeedRange
                      mapLine
                      { mappingSeedRange with
                          Start = mapLine.SourceStart },
                   true) ]
            | _ when // lower part matched
                mapLine.SourceEnd < mappingSeedRange.End
                && mapLine.SourceStart <= mappingSeedRange.Start
                && mapLine.SourceEnd >= mappingSeedRange.Start
                ->
                [ (applyLineToSeedRange
                      mapLine
                      { mappingSeedRange with
                          End = mapLine.SourceEnd },
                   true)
                  ({ mappingSeedRange with
                      Start = mapLine.SourceEnd + 1L },
                   false) ]
            | _ when // middle part matched
                mapLine.SourceStart > mappingSeedRange.Start
                && mapLine.SourceEnd < mappingSeedRange.End
                ->
                [ ({ mappingSeedRange with
                      End = mapLine.SourceStart - 1L },
                   false)
                  (applyLineToSeedRange
                      mapLine
                      { Start = mapLine.SourceStart
                        End = mapLine.SourceEnd },
                   true)
                  ({ mappingSeedRange with
                      Start = mapLine.SourceEnd + 1L },
                   false) ]
            // nothing matched
            | _ -> [ (mappingSeedRange, false) ])
        |> Seq.concat

    let applyMaps maps (seedRanges: seq<SeedRange>) =
        maps
        |> Seq.fold
            (fun currentSeedRanges map ->
                currentSeedRanges
                |> Seq.map (fun seedRange ->
                    map.Lines
                    |> Seq.fold applyMapLine [ (seedRange, false) ]
                    |> Seq.map (fun (value, _) -> value))
                |> Seq.concat)
            seedRanges

    let private CommonPart postProcessSeeds (input: string) =
        input.Split(System.Environment.NewLine + System.Environment.NewLine)
        |> Seq.map (fun chunk ->
            match (chunk |> Seq.takeWhile ((<>) (':')) |> System.String.Concat) with
            | "seeds" ->
                Seeds(
                    chunk.Split(':')
                    |> Seq.item 1
                    |> (fun x -> x.Trim().Split(' '))
                    |> Seq.map int64
                    |> Seq.map (fun value -> { Start = value; End = value })
                    |> postProcessSeeds
                )
            | "seed-to-soil map" -> chunk |> convertChunkToMap Seed Soil
            | "soil-to-fertilizer map" -> chunk |> convertChunkToMap Soil Fertilizer
            | "fertilizer-to-water map" -> chunk |> convertChunkToMap Fertilizer Water
            | "water-to-light map" -> chunk |> convertChunkToMap Water Light
            | "light-to-temperature map" -> chunk |> convertChunkToMap Light Temperature
            | "temperature-to-humidity map" -> chunk |> convertChunkToMap Temperature Humidity
            | "humidity-to-location map" -> chunk |> convertChunkToMap Humidity Location
            | x -> raise (new System.FormatException(x.ToString())))
        |> Seq.fold
            (fun (seeds, maps) chunk ->
                match chunk with
                | Seeds seed -> (seed :: seeds, maps)
                | MapChunk map -> (seeds, map :: maps))
            (List.empty, List.empty)
        |> fun (seedsList, maps) -> (Seq.concat seedsList, orderMaps maps MappingType.Seed List.empty)
        |> fun (seeds, maps) -> seeds |> applyMaps maps
        |> Seq.map (fun seedRange -> seedRange.Start)
        |> Seq.min
        |> string

    let Solution1: Common.Solution = CommonPart id

    let Solution2: Common.Solution =
        CommonPart(fun seeds ->
            seeds
            |> Seq.chunkBySize 2
            |> Seq.map (fun seedRange ->
                { Start = (seedRange |> Seq.head).Start
                  End = (seedRange |> Seq.head).Start + ((seedRange |> Seq.last)).End - 1L }))
