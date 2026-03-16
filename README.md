# AdventOfCode
Advent Of Code, done in F#.
Started as I was curious how far I could push myself by working on this day by day in a language I am not familiar with and finished until Day 12 Part 2, where I couldn't figure out the optimization step (obvious in hindsight though).
It was designed for my own convenience without trying to get overly complicated. It works, but a bit clunky to run.

## Run the project

From the repository root (`/workspaces/AdventOfCode`):

1. Build:

```bash
dotnet build AdventOfCode/AdventOfCode.fsproj
```

2. Run:

Program.fs will run `runAoCYear Real 2023` or `runAoCYear Example 2023`. Real will execute on the input, Example will execute on the examples.

Example mode:
```fsharp
runAoCYear Example 2023
```

Real mode:
```fsharp
runAoCYear Real 2023
```

Then run:

```bash
dotnet run --project AdventOfCode/AdventOfCode.fsproj
```

## Input files

The runner reads input from `AdventOfCode/Inputs/AoC2023/DayXX/input.txt` and examples from `example1.txt`/`example2.txt`.

`input.txt` is your unique input for AoC.

## Add a new day

1. Add `AoC2023/DayNN.fs` with `Solution1` and `Solution2` functions.
2. Add the day to `Runner.fs` in the `aocYears` day list.
3. Add `Inputs/AoC2023/DayNN/input.txt` and example files.

## Add a new year

This is not built out, some menuing would be needed. If done, menuing should also allow selecting Example vs Real modes, and ideally even the day, as it's a waste of time to rerun the logic on every day's input, always.

## Notes

AoC 2023 Day 2 will not finish in a reasonable timeline. If you run this with your own inputs, don't wait for that.