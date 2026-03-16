namespace AoC2023

module Day01 =
    type private ValueSelector =
        | First
        | Last

    type private NumberRecognition =
        | NumericOnly
        | NumericAndTextual

    type private Number = { Textual: string; Numeric: string }
    type private IndexedNumber = { Index: int option; Value: Number }

    let private validNumbers =
        [ { Textual = "one"; Numeric = "1" }
          { Textual = "two"; Numeric = "2" }
          { Textual = "three"; Numeric = "3" }
          { Textual = "four"; Numeric = "4" }
          { Textual = "five"; Numeric = "5" }
          { Textual = "six"; Numeric = "6" }
          { Textual = "seven"; Numeric = "7" }
          { Textual = "eight"; Numeric = "8" }
          { Textual = "nine"; Numeric = "9" } ]

    let private getNumberForLine (numberRecognition: NumberRecognition) (valueSelector: ValueSelector) (line: string) : Number =
        let ignoreTextual (value) =
            match numberRecognition with
            | NumericAndTextual -> value
            | NumericOnly -> None

        let getIndex number line =
            match valueSelector with
            | First -> minOption (line |> indexOf number.Numeric) (ignoreTextual (line |> indexOf number.Textual))
            | Last -> maxOption (line |> lastIndexOf number.Numeric) (ignoreTextual (line |> lastIndexOf number.Textual))

        let getValueFromIndex projection source =
            match valueSelector with
            | First -> Seq.minBy projection source
            | Last -> Seq.maxBy projection source

        validNumbers
        |> Seq.map (fun number ->
            { Index = (line |> getIndex number)
              Value = number })
        |> Seq.filter (fun (value: IndexedNumber) -> value.Index.IsSome)
        |> getValueFromIndex (fun x -> x.Index)
        |> fun x -> x.Value

    let private CommonPart (numberRecognition: NumberRecognition) (input: string) =
        input.Split(System.Environment.NewLine)
        |> Seq.map (fun line -> (+) (line |> getNumberForLine numberRecognition First).Numeric (line |> getNumberForLine numberRecognition Last).Numeric)
        |> Seq.map int
        |> Seq.sum
        |> string

    let Solution1: Common.Solution = CommonPart NumberRecognition.NumericOnly
    let Solution2: Common.Solution = CommonPart NumberRecognition.NumericAndTextual
