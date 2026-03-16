namespace AoC2023

open System.Text.RegularExpressions

module Day02 =
    let private GameIdRegex = Regex("Game (\d+):")
    let private NumberRegex = Regex("\d+")
    let private ColorRegex = Regex("[a-z]+")

    type private Color =
        | Red
        | Green
        | Blue

    type private DiceCount = { Count: int; Color: Color }
    type private Sample = { Dice: seq<DiceCount> }
    type private Game = { Id: int; Samples: seq<Sample> }

    let private possibleMaxDiceCounts =
        [| { Count = 12; Color = Color.Red }
           { Count = 13; Color = Color.Green }
           { Count = 14; Color = Color.Blue } |]

    let private isValidSample (sample: Sample) =
        sample.Dice
        |> Seq.forall (fun dice ->
            possibleMaxDiceCounts
            |> Seq.exists (fun x -> (x.Count >= dice.Count && x.Color = dice.Color)))

    let private parseSamples (line: string) =
        line.Split(":")
        |> Seq.item 1
        |> fun x -> x.Split(";")
        |> Seq.map (fun sample ->
            sample.Split(",")
            |> Seq.map (fun diceCount ->
                { Count = NumberRegex.Match(diceCount).Value |> int
                  Color =
                    match ColorRegex.Match(diceCount).Value with
                    | "red" -> Color.Red
                    | "green" -> Color.Green
                    | "blue" -> Color.Blue
                    | _ -> raise (System.FormatException()) }))
        |> Seq.map (fun x -> { Dice = x })

    let private CommonPart (counter: seq<Game> -> seq<int>) (input: string) =
        input.Split(System.Environment.NewLine)
        |> Seq.map (fun line ->
            { Id = GameIdRegex.Match(line).Groups[1].Value |> int
              Samples = line |> parseSamples })
        |> counter
        |> Seq.sum
        |> string

    let Solution1: Common.Solution =
        CommonPart(fun (games: seq<Game>) ->
            games
            |> Seq.filter (fun x -> x.Samples |> Seq.forall isValidSample)
            |> Seq.map (fun x -> x.Id))

    let Solution2: Common.Solution =
        CommonPart(fun (games: seq<Game>) ->
            games
            |> Seq.map (fun game ->
                game.Samples
                |> Seq.collect (fun sample -> sample.Dice)
                |> Seq.groupBy (fun dice -> dice.Color)
                |> Seq.map (fun (color, dice) ->
                    { Color = color
                      Count = (dice |> Seq.maxBy (fun x -> x.Count)).Count })
                |> Seq.fold (fun state current -> state * current.Count) 1))
