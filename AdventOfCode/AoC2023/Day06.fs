namespace AoC2023

open FSharp.Collections.ParallelSeq

module Day06 =
    type Race = { Time: int64; Distance: int64 }

    let private CommonPart postProcessLines (input: string) =
        input.Split(System.Environment.NewLine)
        |> Array.map (fun line ->
            line.Split(':')
            |> Array.last
            |> fun x -> x.Split(" ")
            |> Array.filter ((<>) (""))
            |> postProcessLines)
        |> Array.transpose
        |> Array.fold
            (fun value column ->
                { Time = column |> Array.head |> int64
                  Distance = column |> Array.last |> int64 }
                |> fun race ->
                    [| 0L .. race.Time |]
                    |> Array.filter (fun buttonHeldLength -> buttonHeldLength * (race.Time - buttonHeldLength) > race.Distance)
                    |> Array.length
                |> (*) value)
            1
        |> string

    let Solution1: Common.Solution = CommonPart id

    let Solution2: Common.Solution =
        CommonPart(fun line -> line |> String.concat "" |> (fun x -> [| x |]))
