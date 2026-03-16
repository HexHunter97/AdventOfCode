namespace AoC2023

open Open.Numeric.Primes

module Day08 =
    let private mapInstruction direction =
        match direction with
        | 'L' -> fst
        | 'R' -> snd
        | _ -> raise (System.FormatException())

    [<TailCall>]
    let rec private traverseMap (instructionsLeft: (string * string -> string) list) originalInstructions (map: Map<string, (string * string)>) step matcher current =
        map
        |> Map.find current
        |> fun next ->
            match current with
            | x when matcher x -> step
            | _ ->
                (match instructionsLeft with
                 | head :: tail when not (tail |> List.isEmpty) -> traverseMap tail originalInstructions map (step + 1) matcher (head next)
                 | head :: _ -> traverseMap (originalInstructions |> Seq.toList) originalInstructions map (step + 1) matcher (head next)
                 | _ -> failwith "")

    let private parseMap =
        Seq.map (fun (line: string) ->
            line.Split(" = ")
            |> fun (parts) ->
                (parts |> Seq.head,
                 parts
                 |> Seq.last
                 |> (fun possibleDirections ->
                     possibleDirections.Trim("( )".ToCharArray()).Split(", ")
                     |> fun directionParts -> (directionParts |> Seq.head, directionParts |> Seq.last))))
        >> Map.ofSeq

    let private CommonPart startMatcher endMatcher (input: string) =
        input.Split(System.Environment.NewLine)
        |> fun lines ->
            ((lines
              |> Seq.head
              |> fun firstLine -> firstLine.ToCharArray() |> Seq.map mapInstruction),
             lines |> Seq.tail |> Seq.tail |> parseMap)
        |> fun (instructions, map) ->
            map
            |> Map.filter (fun position _ -> startMatcher position)
            |> Map.map (fun position _ -> position)
            |> Map.values
            |> Seq.map (traverseMap (instructions |> Seq.toList) instructions map 0 endMatcher)
            |> Seq.map (
                int64
                >> Prime.Factors // 2 2 2 3
                >> Seq.groupBy id
                >> Seq.map (fun (key, values) -> (key, values |> Seq.length))
            )
        |> Seq.fold
            (fun allFactors factors ->
                factors
                |> Seq.fold
                    (fun allFactors (factor, power) ->
                        match allFactors |> Map.tryFind factor with
                        | Some(knownPower) when power <= knownPower -> allFactors
                        | _ -> allFactors |> Map.add factor power)
                    allFactors)
            Map.empty
        |> Map.fold (fun result factor power -> result * (pown factor power)) 1L
        |> string

    let Solution1: Common.Solution = CommonPart ((=) "AAA") ((=) "ZZZ")

    let Solution2: Common.Solution =
        CommonPart (fun value -> value.EndsWith('A')) (fun value -> value.EndsWith('Z'))
