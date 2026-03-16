module Runner

type RunType =
    | Real
    | Example

type Solution =
    | First
    | Second

type Day =
    { Number: int
      Solution1: Common.Solution
      Solution2: Common.Solution }

type AoCYear = { Year: int; Days: seq<Day> }

let private mapSolutionToNumber solution =
    match solution with
    | First -> 1
    | Second -> 2

let private aocYears =
    [ { Year = 2023
        Days =
          [ { Number = 1
              Solution1 = AoC2023.Day01.Solution1
              Solution2 = AoC2023.Day01.Solution2 }
            { Number = 2
              Solution1 = AoC2023.Day02.Solution1
              Solution2 = AoC2023.Day02.Solution2 }
            { Number = 3
              Solution1 = AoC2023.Day03.Solution1
              Solution2 = AoC2023.Day03.Solution2 }
            { Number = 4
              Solution1 = AoC2023.Day04.Solution1
              Solution2 = AoC2023.Day04.Solution2 }
            { Number = 5
              Solution1 = AoC2023.Day05.Solution1
              Solution2 = AoC2023.Day05.Solution2 }
            { Number = 6
              Solution1 = AoC2023.Day06.Solution1
              Solution2 = AoC2023.Day06.Solution2 }
            { Number = 7
              Solution1 = AoC2023.Day07.Solution1
              Solution2 = AoC2023.Day07.Solution2 }
            { Number = 8
              Solution1 = AoC2023.Day08.Solution1
              Solution2 = AoC2023.Day08.Solution2 }
            { Number = 9
              Solution1 = AoC2023.Day09.Solution1
              Solution2 = AoC2023.Day09.Solution2 }
            { Number = 10
              Solution1 = AoC2023.Day10.Solution1
              Solution2 = AoC2023.Day10.Solution2 }
            { Number = 11
              Solution1 = AoC2023.Day11.Solution1
              Solution2 = AoC2023.Day11.Solution2 }
            { Number = 12
              Solution1 = AoC2023.Day12.Solution1
              Solution2 = AoC2023.Day12.Solution2 } ] } ]

let private fileTemplate = @".\Inputs\AoC{0}\Day{1:00}\"

let private getFileContents =
    System.IO.File.ReadAllText >> (fun value -> value.Trim())

let private getPathFor year day =
    fun x -> System.IO.Path.Combine(System.String.Format(fileTemplate, year, day), x)

let private getInputFor year day _ =
    getFileContents (getPathFor year day "input.txt")

let private getExampleFor year day solution =
    getFileContents (getPathFor year day $"example{mapSolutionToNumber solution}.txt")

let private inputGetter runType =
    match runType with
    | Real -> getInputFor
    | Example -> getExampleFor

let private measure action =
    System.Diagnostics.Stopwatch.StartNew()
    |> (fun sw -> (action (), sw.Stop(), sw.Elapsed))
    |> (fun (actionRes, _, elapsed) -> (actionRes, elapsed))

let private printAoCSolution runType year day solution solutionFunc =
    fun () -> (inputGetter runType year day solution |> solutionFunc)
    |> measure
    |> fun (result, elapsed) -> printfn "AoC %i Day %2i Solution %i\t%s" year day (mapSolutionToNumber solution) $"{result, -15} {elapsed.TotalSeconds |> int, 5}.{elapsed.Milliseconds:D3}s"

    |> ignore

let runAoCSolution runType year day (solution: Solution) =
    aocYears
    |> Seq.find (fun aocYear -> aocYear.Year = year)
    |> (fun aocYear -> aocYear.Days)
    |> Seq.find (fun aocDay -> aocDay.Number = day)
    |> fun aocDay ->
        match solution with
        | Solution.First -> aocDay.Solution1
        | Solution.Second -> aocDay.Solution2
    |> printAoCSolution runType year day solution

let runAoCDay runType year day =
    runAoCSolution runType year day Solution.First
    runAoCSolution runType year day Solution.Second

let runAoCYear runType year =
    aocYears
    |> Seq.find (fun aocYear -> aocYear.Year = year)
    |> fun aocYear -> aocYear.Days
    |> Seq.iter (fun day -> runAoCDay runType year day.Number)

let runAoC runType =
    aocYears |> Seq.iter (fun aocYear -> runAoCYear runType aocYear.Year)
