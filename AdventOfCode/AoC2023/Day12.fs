namespace AoC2023

open FSharp.Collections.ParallelSeq

module Day12 =
    type private SpringState =
        | Damaged
        | Operational

    type private SpringKnowledgeState =
        | Known of SpringState
        | Unknown

    type private ConditionRecord =
        { Springs: SpringKnowledgeState list
          SpringGroups: int list }

    let private parseField value =
        match value with
        | '#' -> Known Damaged
        | '.' -> Known Operational
        | _ -> Unknown

    [<TailCallAttribute>]
    let rec private getNumberOfValidStates springs (springGroups: int list) damagedSpringsCount (continuations: (unit -> int64) list) =
        let foldContinuations isCurrentStateValid continuations =
            (if isCurrentStateValid then 1L else 0)
            + (continuations |> Seq.sumBy (fun x -> x ()))

        match springs with
        | spring :: springsTail when spring = Known Operational ->
            match damagedSpringsCount with
            | 0 -> getNumberOfValidStates springsTail springGroups damagedSpringsCount continuations
            | _ ->
                springGroups
                |> Seq.tryHead
                |> fun currentSpringGroup ->
                    if currentSpringGroup = Some damagedSpringsCount then
                        getNumberOfValidStates springsTail (springGroups |> List.tail) 0 continuations
                    else
                        continuations |> foldContinuations false
        | spring :: springsTail when spring = Known Damaged -> getNumberOfValidStates springsTail springGroups (damagedSpringsCount + 1) continuations
        | spring :: springsTail when spring = Unknown ->
            getNumberOfValidStates
                (Known Damaged :: springsTail)
                springGroups
                damagedSpringsCount
                ((fun () -> getNumberOfValidStates (Known Operational :: springsTail) springGroups damagedSpringsCount [])
                 :: continuations)
        | _ ->
            match springGroups with
            | springGroupHead :: [] when springGroupHead = damagedSpringsCount -> continuations |> foldContinuations true
            | [] when damagedSpringsCount = 0 -> continuations |> foldContinuations true
            | _ -> continuations |> foldContinuations false

    let private getNumberOfValidStatesFromRecord (record: ConditionRecord) : int64 =
        getNumberOfValidStates record.Springs record.SpringGroups 0 []

    let private CommonPart (postProcessRecord: ConditionRecord -> ConditionRecord) (input: string) =
        let k =
            input.Split(System.Environment.NewLine)
            |> Array.map (fun (line: string) ->
                line.Split(" ")
                |> fun splitLine ->
                    { Springs = (splitLine |> Array.head).ToCharArray() |> Array.map parseField |> List.ofArray
                      SpringGroups =
                        splitLine
                        |> Array.last
                        |> (fun x -> x.Split(","))
                        |> Array.map int
                        |> List.ofArray }
                |> postProcessRecord)

        let length = k |> Array.length
        let mutable progress = 0

        k
        |> PSeq.withDegreeOfParallelism 500
        |> PSeq.withExecutionMode System.Linq.ParallelExecutionMode.ForceParallelism
        |> PSeq.sumBy (fun record ->
            let result = record |> getNumberOfValidStatesFromRecord
            printfn "Calculated %i/%i at %A" (System.Threading.Interlocked.Increment(&progress)) length System.DateTime.Now.TimeOfDay
            result)
        |> string

    let Solution1: Common.Solution = fun x -> "XX" //CommonPart id

    let Solution2: Common.Solution =
        CommonPart(fun record ->
            // TODO: forgot to add ? between replicas!!!
            { SpringGroups = record.SpringGroups |> Seq.replicate 5 |> List.concat
              Springs =
                record.Springs
                |> Seq.replicate 5
                |> Seq.fold (fun state current -> state @ (Unknown :: current)) [] })

module Day12_O =
    type private SpringState =
        | Damaged
        | Operational

    type private SpringKnowledgeState =
        | Known of SpringState
        | Unknown

    type private ConditionRecord =
        { Springs: (int * SpringKnowledgeState) array
          SpringGroups: int list }

    type private CalculationDataContainer =
        { Record: ConditionRecord
          UnknownFields: (int * SpringKnowledgeState) array
          UnknownFieldsCount: int
          ExpectedNumberOfDamagedSprings: int }

    let private parseField value =
        match value with
        | '#' -> Known Damaged
        | '.' -> Known Operational
        | _ -> Unknown

    let private calculateSpringGroup springs =
        springs
        |> Seq.fold
            (fun (count, list) (_, current) ->
                match current with
                | Known Operational -> (0, (if count <> 0 then count :: list else list))
                | _ -> (count + 1, list))
            (0, [])
        |> (fun (count, list) -> if count <> 0 then count :: list else list)

    let private getNumberOfValidStates unknownsDataSource calculationDataSource =
        unknownsDataSource
        |> Seq.take (pown 2 calculationDataSource.UnknownFieldsCount)
        |> Seq.filter (fun value ->
            value
            |> Array.sumBy (fun (_, bit) -> if bit then 1 else 0)
            |> fun numberOfBits -> numberOfBits = calculationDataSource.ExpectedNumberOfDamagedSprings)
        |> Seq.fold
            (fun count unknownStates ->
                calculationDataSource.UnknownFields
                |> Seq.zip unknownStates
                |> Seq.map (fun ((_, shouldBeDamaged), (index, _)) -> (index, (if shouldBeDamaged then Known Damaged else Known Operational)))
                |> Seq.append calculationDataSource.Record.Springs
                |> Seq.sortBy (fun (index, value) -> (index, (if value = Unknown then 1 else 0)))
                |> Seq.distinctBy fst
                |> calculateSpringGroup
                |> List.rev
                |> List.forall2 (=) calculationDataSource.Record.SpringGroups
                |> fun isMatching -> if isMatching then count + 1 else count)
            0

    let private CommonPart postProcessRecord (input: string) =
        input.Split(System.Environment.NewLine)
        |> Array.map (fun (line: string) ->
            line.Split(" ")
            |> fun splitLine ->
                { Springs = (splitLine |> Array.head).ToCharArray() |> Array.map parseField |> Array.indexed
                  SpringGroups =
                    splitLine
                    |> Array.last
                    |> (fun x -> x.Split(","))
                    |> Array.map int
                    |> List.ofArray }
            |> postProcessRecord
            |> fun record ->
                { Record = record
                  UnknownFields = record.Springs |> Array.filter (snd >> ((=) Unknown))
                  UnknownFieldsCount = -1
                  ExpectedNumberOfDamagedSprings =
                    (record.SpringGroups |> List.sum)
                    - (record.Springs |> Seq.filter (snd >> ((=) (Known Damaged))) |> Seq.length) }
            |> fun calculationDataContainer ->
                { calculationDataContainer with
                    UnknownFieldsCount = calculationDataContainer.UnknownFields |> Array.length })
        |> fun lines ->
            lines
            |> PSeq.maxBy (fun calculationDataContainer -> calculationDataContainer.UnknownFieldsCount)
            |> (fun containerWithLargestUnknownFieldsCount ->
                [| 0 .. (pown 2 containerWithLargestUnknownFieldsCount.UnknownFieldsCount) - 1 |]
                |> Array.map (fun number ->
                    System.Collections.BitArray [| number |]
                    |> fun (bitArray: System.Collections.BitArray) ->
                        [| 0 .. containerWithLargestUnknownFieldsCount.UnknownFieldsCount - 1 |]
                        |> Array.map (fun index -> (index, bitArray.Get index))))
            |> fun unknownsDataSource -> lines |> PSeq.sumBy (getNumberOfValidStates unknownsDataSource)
        |> string

    let Solution1: Common.Solution = CommonPart id

    let Solution2: Common.Solution = fun x -> "X"
(*
    CommonPart(fun record ->
        // TODO: forgot to add ? between replicas!!!
        { SpringGroups = record.SpringGroups |> Seq.replicate 5 |> List.concat
            Springs = record.Springs |> Seq.replicate 5 |> Array.concat })
*)

(*
    IDEAS:
    - bruteforce till it breaks    
*)
