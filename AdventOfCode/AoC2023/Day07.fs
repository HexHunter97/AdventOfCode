namespace AoC2023

module Day07 =
    type HandType =
        | FiveOfAKind = 6
        | FourOfAKind = 5
        | FullHouse = 4
        | ThreeOfAKind = 3
        | TwoPair = 2
        | OnePair = 1
        | HighCard = 0

    [<CustomEquality>]
    [<CustomComparison>]
    type Hand =
        { Type: HandType
          Cards: int array }

        override this.Equals other =
            ((this :> System.IComparable).CompareTo other) = 0

        override this.GetHashCode() = 0

        interface System.IComparable with
            member this.CompareTo(other: obj) =
                match other with
                | :? Hand as otherHand -> (this :> System.IComparable<Hand>).CompareTo otherHand
                | _ -> -1

        interface System.IComparable<Hand> with
            member this.CompareTo(otherHand: Hand) =
                match otherHand.Type.CompareTo(this.Type) with
                | 0 ->
                    Seq.zip this.Cards otherHand.Cards
                    |> Seq.tryFind (fun (first, second) -> first <> second)
                    |> Option.defaultValue (0, 0)
                    |> fun (first, second) -> -(first.CompareTo second)
                | x -> x

    type Bid = { Hand: Hand; BidAmount: int64 }

    let cards =
        Map(
            seq {
                ('-', -1)
                ('2', 0)
                ('3', 1)
                ('4', 3)
                ('5', 4)
                ('6', 5)
                ('7', 6)
                ('8', 7)
                ('9', 8)
                ('T', 9)
                ('J', 10)
                ('Q', 11)
                ('K', 12)
                ('A', 13)
            }
        )

    let getCard jIsJoker symbol =
        cards
        |> Map.find (
            match symbol with
            | 'J' when jIsJoker -> '-'
            | x -> x
        )

    let getHandType jIsJoker (hand: string) =
        match
            (hand.ToCharArray()
             |> Array.groupBy id
             |> Array.map (fun (card, values) -> (card, values |> Array.length))
             |> (fun values ->
                 match jIsJoker with
                 | true when values |> Array.exists (fst >> ((=) 'J')) && values |> Array.length >= 2 ->
                     values
                     |> Array.sortByDescending snd
                     |> Array.filter (fun (card, _) -> card <> 'J')
                     |> (fun filteredValues ->
                         [ (filteredValues |> Array.head |> fst,
                            (filteredValues |> Array.head |> snd)
                            + (values
                               |> Array.tryFind (fst >> ((=) 'J'))
                               |> Option.defaultValue ('x', 0)
                               |> snd)) ]
                         |> Seq.append filteredValues
                         |> Seq.tail)
                 | _ -> values)
             |> Seq.sortByDescending (fun (_, count) -> count)
             |> Seq.map snd
             |> Seq.toList)
        with
        | first :: _ when first = 5 -> HandType.FiveOfAKind
        | first :: _ when first = 4 -> HandType.FourOfAKind
        | first :: second :: _ when first = 3 && second = 2 -> HandType.FullHouse
        | first :: _ when first = 3 -> HandType.ThreeOfAKind
        | first :: second :: _ when first = 2 && second = 2 -> HandType.TwoPair
        | first :: _ when first = 2 -> HandType.OnePair
        | _ -> HandType.HighCard

    let getHand jIsJoker hand =
        { Type = getHandType jIsJoker hand
          Cards = hand.ToCharArray() |> Array.map (getCard jIsJoker) }

    let private CommonPart jIsJoker (input: string) =
        input.Split(System.Environment.NewLine)
        |> Seq.map (fun line ->
            line.Split(" ")
            |> fun parts ->
                { Hand = parts |> Seq.head |> getHand jIsJoker
                  BidAmount = parts |> Seq.last |> int64 })
        |> Seq.sortDescending
        |> Seq.indexed
        |> Seq.map (fun (index, value) -> ((index + 1) |> int64) * value.BidAmount)
        |> Seq.sum
        |> string

    let Solution1: Common.Solution = CommonPart false

    let Solution2: Common.Solution = CommonPart true
