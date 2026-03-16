namespace AoC2023

module Day10 =
    type private PipeType =
        | Horizontal
        | Vertical
        | SouthToWest
        | SouthToEast
        | NorthToWest
        | NorthToEast

    type private TileType =
        | Pipe of PipeType
        | Start of PipeType option
        | Empty

    let private mapTileType tile =
        match tile with
        | '|' -> Pipe Vertical
        | '-' -> Pipe Horizontal
        | 'L' -> Pipe NorthToEast
        | 'J' -> Pipe NorthToWest
        | '7' -> Pipe SouthToWest
        | 'F' -> Pipe SouthToEast
        | 'S' -> Start None
        | _ -> Empty

    let private getOffsetForConnectionsFor pipe =
        match pipe with
        | Vertical -> [ (0L, 1L); (0L, -1L) ]
        | Horizontal -> [ (1L, 0L); (-1L, 0L) ]
        | NorthToEast -> [ (0L, -1L); (1L, 0L) ]
        | NorthToWest -> [ (0L, -1L); (-1, 0L) ]
        | SouthToEast -> [ (0L, 1L); (1L, 0L) ]
        | SouthToWest -> [ (0L, 1L); (-1L, 0L) ]

    let private pipeTypesWithOffsets =
        [ Vertical; Horizontal; NorthToEast; NorthToWest; SouthToEast; SouthToWest ]
        |> Seq.map (fun pipe -> (pipe, getOffsetForConnectionsFor pipe))
        |> Map.ofSeq

    let private isPipeOnFieldPointingTo position (pipePosition, pipeType) =
        pipeTypesWithOffsets
        |> Map.find pipeType
        |> Seq.fold (fun isPointingToStart offset -> isPointingToStart || (offsetPoint pipePosition offset) = position) false

    let private identifyStartPipeType map =
        map
        |> Map.findKey (fun _ value -> value = Start None)
        |> fun startPosition ->
            getEdgeAdjacentFields map startPosition
            |> Seq.filter (fun (adjacentField, tileType) ->
                match tileType with
                | Pipe pipeType -> isPipeOnFieldPointingTo startPosition (adjacentField, pipeType)
                | _ -> false)
            |> Seq.fold
                (fun validPipes (adjacentPoint, _) ->
                    validPipes
                    |> Seq.filter (fun pipeType -> isPipeOnFieldPointingTo adjacentPoint (startPosition, pipeType)))
                (pipeTypesWithOffsets |> Map.keys |> seq<PipeType>)
            |> Seq.exactlyOne
            |> fun pipeType -> (startPosition, pipeType, map |> Map.add startPosition (Pipe pipeType))

    let private getNextPosition (previousPosition: Point) (currentPosition: Point) map =
        match map |> Map.find currentPosition with
        | Pipe pipeType ->
            match pipeType with
            | (Vertical | Horizontal) -> currentPosition + (currentPosition - previousPosition)
            | NorthToEast when currentPosition.Y > previousPosition.Y ->
                { currentPosition with
                    X = currentPosition.X + 1L }
            | NorthToWest when currentPosition.Y > previousPosition.Y ->
                { currentPosition with
                    X = currentPosition.X - 1L }
            | (NorthToEast | NorthToWest) ->
                { currentPosition with
                    Y = currentPosition.Y - 1L }
            | SouthToEast when currentPosition.Y < previousPosition.Y ->
                { currentPosition with
                    X = currentPosition.X + 1L }
            | SouthToWest when currentPosition.Y < previousPosition.Y ->
                { currentPosition with
                    X = currentPosition.X - 1L }
            | (SouthToEast | SouthToWest) ->
                { currentPosition with
                    Y = currentPosition.Y + 1L }
        | _ -> failwithf "Landed outside of pipes! %A -> %A" previousPosition currentPosition

    [<TailCall>]
    let rec private walkLoop goal map previous current accummulated accummulator =
        accummulator current previous accummulated
        |> fun accummulatedValue ->
            match getNextPosition previous current map with
            | nextPosition when nextPosition = goal -> accummulatedValue
            | nextPosition -> walkLoop goal map current nextPosition accummulatedValue accummulator

    [<TailCall>]
    let rec private takeUntil offset current map loop foundItems =
        if map |> Map.tryFind current = None then
            failwith "Where the fuck are we?!"

        match loop |> Set.contains current with
        | false ->
            foundItems
            |> Set.add current
            |> takeUntil offset (offsetPoint current offset) map loop
        | true -> foundItems

    // not conventional rotation rules, but in my indexing, this is what works
    let private rotateOffsetRight (x, y) = (-y, x)
    let private rotateOffsetLeft (x, y) = (y, -x)

    let private accummulateTakenPoints map loop currentPosition previousPosition (foundPositions: Set<Point>, lastRightDirection) =
        match map |> Map.find currentPosition with
        | Pipe pipeType ->
            getNextPosition previousPosition currentPosition map
            |> fun value -> (value.X - currentPosition.X, value.Y - currentPosition.Y)
            |> fun nextDirection ->
                match pipeType with
                | (Vertical | Horizontal) -> ([ lastRightDirection ], lastRightDirection)
                | _ when lastRightDirection = nextDirection -> ([], rotateOffsetRight lastRightDirection)
                | _ -> ([ lastRightDirection; rotateOffsetLeft lastRightDirection ], rotateOffsetLeft lastRightDirection)
                |> fun (directionsToLook, newRightDirection) ->
                    (directionsToLook
                     |> List.map (fun offset -> takeUntil offset (offsetPoint currentPosition offset) map loop Set.empty)
                     |> (fun (pointsList: Set<Point> list) -> foundPositions :: pointsList)
                     |> Set.unionMany,
                     newRightDirection)
        | _ -> failwithf "Accummulation on invalid tiles! %A -> %A" previousPosition currentPosition

    let private CommonPart getCountFromLoop (input: string) : string =
        input.Split(System.Environment.NewLine)
        |> Seq.indexed
        |> Seq.map (fun (coordY, line) ->
            line.ToCharArray()
            |> Seq.indexed
            |> Seq.map (fun (coordX, char) -> ({ X = coordX; Y = coordY }, mapTileType char)))
        |> Seq.concat
        |> Map.ofSeq
        |> identifyStartPipeType
        |> fun (start, startPipeType, map) ->
            (pipeTypesWithOffsets
             |> Map.find startPipeType
             |> Seq.head
             |> offsetPoint start
             |> fun previous -> walkLoop start map previous start [ start ] (fun current _ loop -> current :: loop)
             |> Set.ofSeq,
             map)
        |> getCountFromLoop
        |> string

    let Solution1: Common.Solution =
        CommonPart(fst >> Seq.length >> (fun loopLength -> (loopLength + 1) / 2))

    let Solution2: Common.Solution =
        CommonPart(fun (loop, tileMap) ->
            loop
            |> Seq.sort
            |> Seq.head
            |> fun start -> walkLoop start tileMap (offsetPoint start (0L, 1L)) start (Set.empty, (1L, 0L)) (accummulateTakenPoints tileMap loop)
            |> fst
            |> Set.count)

(*
                TODO:
                # found leftmost, then highest point, must be SouthToEast AND right of it is inside the loop
                - walk on loop FROM top left corner
                - looking down the loop forward, take the right field and take until we reach the start again
                -- consider all found fields internal
                -- keep track of direction, keep rotating it based on 
                - count internal fields
            *)
