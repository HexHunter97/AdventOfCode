namespace AoC2023


module Day04 =
    type ScratchCard =
        { Numbers: seq<int>
          WinningNumbers: seq<int>
          Value: int }

    let private getWinningNumbersCount scratchCard =
        scratchCard.Numbers
        |> Seq.filter (fun number -> scratchCard.WinningNumbers |> Seq.exists ((=) number))
        |> Seq.length

    let private CommonPart valueFunction (input: string) =
        input.Split(System.Environment.NewLine)
        |> Seq.map (fun line -> line.Split(":") |> Seq.last)
        |> Seq.map (fun line ->
            line.Split('|')
            |> Seq.map (fun part -> part.Split(" ") |> Seq.filter ((<>) "") |> Seq.map int))
        |> Seq.map (fun line ->
            { Numbers = Seq.head line
              WinningNumbers = Seq.last line
              Value = 0 })
        |> valueFunction
        |> Seq.sumBy (fun scratchCard -> scratchCard.Value)
        |> string

    let Solution1: Common.Solution =
        CommonPart(fun scratchCards ->
            scratchCards
            |> Seq.map (fun scratchCard ->
                { scratchCard with
                    Value = scratchCard |> getWinningNumbersCount |> (fun count -> pown 2 (count - 1)) }))

    let rec updateScratchCardValues currentIndex indexedScratchCards =
        indexedScratchCards
        |> Seq.find (fun (index, _) -> index = currentIndex)
        |> fun (_, currentScratchCard) ->
            currentScratchCard
            |> getWinningNumbersCount
            |> fun winningNumbersCount ->
                indexedScratchCards
                |> Seq.skip (currentIndex + 1) (*offByOne?*)
                |> Seq.take winningNumbersCount
            |> Seq.map (fun (index, scratchCard) ->
                (index,
                 { scratchCard with
                     Value = currentScratchCard.Value + scratchCard.Value }))
        |> Seq.fold (fun updatedScratchCards (indexToUpdate, valueToUpdate) -> updatedScratchCards |> Seq.updateAt indexToUpdate (indexToUpdate, valueToUpdate)) indexedScratchCards
        |> fun updatedIndexedScratchCards ->
            match currentIndex + 1 with
            | overIndexed when (Seq.length updatedIndexedScratchCards) <= overIndexed -> updatedIndexedScratchCards
            | nextIndex -> updateScratchCardValues (nextIndex) updatedIndexedScratchCards

    let Solution2: Common.Solution =
        CommonPart(fun scratchCards ->
            scratchCards
            |> Seq.map (fun scratchCard -> { scratchCard with Value = 1 })
            |> Seq.indexed
            |> updateScratchCardValues 0
            |> Seq.map (fun (_, value) -> value))
