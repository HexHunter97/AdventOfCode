[<AutoOpenAttribute>]
module Common

open System

type Solution = string -> string

let tryParseInt (input: (string)) : int option =
    Int32.TryParse input
    |> fun (result, output) ->
        match result with
        | true -> Some(output)
        | false -> None

let isInt (input: string) : bool =
    Int32.TryParse input |> fun (result, _) -> result

let indexOf (value: string) (input: string) : int option =
    match input.IndexOf(value) with
    | -1 -> None
    | _ as x -> Some(x)

let lastIndexOf (value: string) (input: string) : int option =
    match input.LastIndexOf(value) with
    | -1 -> None
    | _ as x -> Some(x)

let private binaryOpIfNeitherNone binaryOp e1 e2 =
    match e1 with
    | None -> e2
    | _ ->
        match e2 with
        | None -> e1
        | _ -> binaryOp e1 e2

let minOption e1 e2 = binaryOpIfNeitherNone min e1 e2
let maxOption e1 e2 = binaryOpIfNeitherNone max e1 e2

let peekOne input =
    printfn "%A" input
    input

let peek (input: seq<'a>) : seq<'a> =
    input |> Seq.iter (fun x -> printfn "%A" x)
    input

let private peekOf toSeq ofSeq = toSeq >> peek >> ofSeq

let peekMap input = input |> peekOf Map.toSeq Map.ofSeq
let peekList<'a> (input: 'a list) : 'a list = input |> peekOf List.toSeq List.ofSeq

let peekWithSeparator separator =
    peek
    >> (fun input ->
        printfn separator
        input)

let log value input =
    printfn "%s" value
    input

type Point =
    { X: int64
      Y: int64 }

    static private apply operation point1 point2 =
        { X = operation point1.X point2.X
          Y = operation point1.Y point2.Y }

    static member (+)((point1: Point), (point2: Point)) = Point.apply (+) point1 point2
    static member (-)((point1: Point), (point2: Point)) = Point.apply (-) point1 point2
    static member length this = abs this.X + abs this.Y

let offsetPoint (point: Point) (offsetX, offsetY) =
    { X = point.X + offsetX
      Y = point.Y + offsetY }

let getOffsetFields<'a> (offsets: seq<int64 * int64>) (schema: Map<Point, 'a>) (point: Point) =
    offsets
    |> Seq.map (offsetPoint point)
    |> Seq.map (fun searchPoint -> (searchPoint, Map.tryFind searchPoint schema))
    |> Seq.map (fun (point, value) ->
        match value with
        | Some _ -> Some(point, value.Value)
        | None -> None)
    |> Seq.filter Option.isSome
    |> Seq.map Option.get

let edgeAdjacentFieldOffset = [ (1L, 0L); (0L, 1L); (0L, -1L); (-1L, 0L) ]
let diagonallyAdjacentFieldOffset = [ (1L, 1L); (1L, -1L); (-1L, 1L); (-1L, -1L) ]
let adjacentFieldOffsets = edgeAdjacentFieldOffset @ diagonallyAdjacentFieldOffset

let getEdgeAdjacentFields<'a> = getOffsetFields<'a> edgeAdjacentFieldOffset

let getDiagonallyAdjacentFields<'a> =
    getOffsetFields<'a> diagonallyAdjacentFieldOffset

let getAdjacentFields<'a> = getOffsetFields<'a> adjacentFieldOffsets
