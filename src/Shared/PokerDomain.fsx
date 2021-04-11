// 5 cards -> Poker hand

open System

[<CustomComparison; StructuralEquality>]
type Suit =
    | Hearts
    | Spades
    | Diamonds
    | Clubs

    interface IComparable with
        member this.CompareTo(obj) = 0


type CardValue =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

module CardValue =
    let allValues =
        [| Two
           Three
           Four
           Five
           Six
           Seven
           Eight
           Nine
           Ten
           Jack
           Queen
           King
           Ace |]

    let getIntValue cardValue =
        Array.IndexOf(allValues, cardValue) + 2

type Card = { Suit: Suit; Value: CardValue }

let aceOfHearts = { Suit = Hearts; Value = Ace }
let fiveOfHearts = { Suit = Hearts; Value = Five }
let fourOfDiamonds = { Suit = Diamonds ; Value = Four }
let jackOfDiamonds = { Suit = Diamonds ; Value = Jack }
let jackOfHearts = { Suit = Hearts ; Value = Jack }
let queenOfDiamonds = { Suit = Diamonds ; Value = Queen }
let twoOfSpades = { Suit = Spades; Value = Two }
let threeOfSpades = { Suit = Spades; Value = Three }
let sixOfSpades = { Suit = Spades; Value = Six }
let kingOfSpades = { Suit = Spades; Value = King }
let nineOfSpades = { Suit = Spades; Value = Nine }
let tenOfSpades = { Suit = Spades; Value = Ten }
let kingOfDiamonds = { Suit = Diamonds; Value = King }
let kingOfHearts = { Suit = Hearts; Value = King }
let kingOfClubs = { Suit = Clubs; Value = King }

[<RequireQualifiedAccess>]
module FiveCards =
    type FiveCards = private FiveCards of Card list
    let fromCards (cards: Card list) =
        if cards |> List.length = 5 then
            cards
            |> FiveCards
            |> Some
        else
            None

    let (|FiveCards|) (FiveCards cards) = FiveCards cards

    let testData =
        [ twoOfSpades
          threeOfSpades
          kingOfSpades
          sixOfSpades
          aceOfHearts ]
        |> FiveCards

    let straightCards =
        [ twoOfSpades
          threeOfSpades
          fiveOfHearts
          sixOfSpades
          fourOfDiamonds ]
        |> FiveCards

    let wheelStraightCards =
        [ twoOfSpades
          threeOfSpades
          fiveOfHearts
          aceOfHearts
          fourOfDiamonds ]
        |> FiveCards

    let suitedCards =
        [ twoOfSpades
          threeOfSpades
          kingOfSpades
          sixOfSpades
          nineOfSpades ]
        |> FiveCards

    let broadwayCards =
        [ jackOfDiamonds
          queenOfDiamonds
          kingOfSpades
          aceOfHearts
          tenOfSpades ]
        |> FiveCards

    let fourOfAKind =
        [ jackOfDiamonds
          kingOfClubs
          kingOfSpades
          kingOfHearts
          kingOfDiamonds ]
        |> FiveCards

    let fullHouse =
        [ jackOfDiamonds
          jackOfHearts
          kingOfSpades
          kingOfHearts
          kingOfDiamonds ]
        |> FiveCards

    let twoPair =
        [ jackOfDiamonds
          jackOfHearts
          kingOfSpades
          kingOfHearts
          aceOfHearts ]
        |> FiveCards

    let pair =
        [ jackOfDiamonds
          nineOfSpades
          kingOfSpades
          kingOfHearts
          aceOfHearts ]
        |> FiveCards

    let threeOfAKind =
        [ jackOfDiamonds
          twoOfSpades
          kingOfSpades
          kingOfHearts
          kingOfDiamonds ]
        |> FiveCards


type SortedCards = SortedCards of Card list
type SortedFiveCards = SortedFiveCards of Card list
type SuitedFiveCards = SuitedFiveCards of Card list

type Pair = { Value: CardValue; Kickers: SortedCards }
type TwoPair = { HighestValue: CardValue; LowestValue: CardValue; Kicker: CardValue }
type ThreeOfAKind = { Value: CardValue; Kickers: SortedCards }
type FullHouse = { ThreeOfAKind: CardValue; Pair: CardValue }
type FourOfAKind = { Value: CardValue; Kicker: CardValue }
type SortedSuitedFiveCards = SortedSuitedFiveCards of Card list

type PokerHand =
    | HighCard of SortedFiveCards
    | Pair of Pair
    | TwoPair of TwoPair
    | ThreeOfAKind of ThreeOfAKind
    | Straight of SortedFiveCards
    | Flush of SuitedFiveCards
    | FullHouse of FullHouse
    | FourOfAKind of FourOfAKind
    | StraightFlush
    | RoyalFlush

type PokerHandSolver = FiveCards.FiveCards -> PokerHand

let isFlush cards=
    let suits =
        cards
        |> List.map (fun card -> card.Suit)

    suits
    |> List.forall ((=) (suits |> List.head))

let (|Suited|_|) (FiveCards.FiveCards cards) =
    if isFlush cards then
        Suited Some cards
    else
        Suited None

let (|Sorted|) cards =
    Sorted List.sortDescending cards

let isStraight (FiveCards.FiveCards (Sorted cards)) =
    let rec inner cards =
        match cards with
        | first::second::tail ->
            if first - second = 1 then
                inner (second::tail)
            else
                false

        | _ -> true

    let sortedCardValues =
        cards
        |> List.map (fun (card: Card) -> card.Value)
        |> (|Sorted|)

    match sortedCardValues with
    | [ Ace; Five; Four; Three; Two ] -> true
    | _ ->
        sortedCardValues
        |> List.map CardValue.getIntValue
        |> inner

let (|Straight|_|) fiveCards =
    if isStraight fiveCards then
        Straight Some fiveCards
    else
        Straight None

    // [9; 8; 7; 6; 5]
    // [5; 6; 7; 8; 9]
    // [1; 1; 1; 1]
    // A, 2, 3, 4, 5 -> Wheel
    // A, K, Q, J, 10 -> Broadway

let isBroadway (Sorted (cards: Card list)) =
    match cards |> List.map (fun c -> c.Value) with
    | [ Ace; King; Queen; Jack; Ten ] -> true
    | _ -> false

let (|Broadway|_|) (FiveCards.FiveCards (Sorted (cards: Card list))) =
    if isBroadway cards then
        Broadway Some ()
    else
        None

type Occurrences = Occurrences of (CardValue * int) list
let getOccurrences (FiveCards.FiveCards (Sorted cards)): Occurrences  =
    cards
    |> List.map (fun c -> c.Value)
    |> List.countBy id
    |> List.sortByDescending snd
    |> Occurrences

let getKickers (Occurrences occurrences) =
    occurrences
    |> List.filter (snd >> (=) 1)
    |> List.map fst
    |> List.sortByDescending CardValue.getIntValue

let (|IsFourOfAKind|_|) (Occurrences occurrences as wrapped) =
    occurrences
    |> List.head
    |> fun (value, number) ->
        if number = 4 then
            IsFourOfAKind Some (value, getKickers wrapped)
        else IsFourOfAKind None

let (|IsFullHouse|_|) (Occurrences occurrences) =
    match occurrences with
    | [ (topSetValue,3); (bottomSetValue,2) ] -> IsFullHouse Some (topSetValue, bottomSetValue)
    | _ -> IsFullHouse None

let (|IsThreeOfAKind|_|) (Occurrences occurrences as wrapped) =
    match occurrences with
    | (setValue,3)::_ -> IsThreeOfAKind Some (setValue, getKickers wrapped)
    | _ -> IsThreeOfAKind None

let (|IsTwoPair|_|) (Occurrences occurrences) =
    match occurrences with
    | [ (topSetValue,2); (bottomSetValue,2); (kicker,1) ] -> IsTwoPair Some (topSetValue, bottomSetValue, kicker)
    | _ -> IsTwoPair None

let (|IsPair|_|) (Occurrences occurrences as wrapped) =
    match occurrences with
    | (pairValue,2)::tail when tail |> List.map snd = [1;1;1] -> IsPair Some (pairValue, getKickers wrapped)
    | _ -> IsPair None

let (|IsHighCard|_|) (Occurrences occurrences as wrapped) =
    match occurrences with
    | x when x |> List.map snd = [1;1;1;1;1] -> IsHighCard Some (getKickers wrapped)
    | _ -> IsHighCard None

let isHighCard fiveCards =
    let occurrences = getOccurrences fiveCards

    match occurrences with
    | IsHighCard _ -> true
    | _ -> false

let isPair fiveCards =
    let occurrences = getOccurrences fiveCards

    match occurrences with
    | IsPair _ -> true
    | _ -> false

let isTwoPair fiveCards =
    let occurrences = getOccurrences fiveCards

    match occurrences with
    | IsTwoPair _ -> true
    | _ -> false

let isThreeOfAKind fiveCards =
    let occurrences = getOccurrences fiveCards

    match occurrences with
    | IsThreeOfAKind _ -> true
    | _ -> false

let isFullHouse fiveCards =
    let occurrences = getOccurrences fiveCards

    match occurrences with
    | IsFullHouse _ -> true
    | _ -> false

let isFourOfAKind fiveCards =
    let occurrences = getOccurrences fiveCards

    match occurrences with
    | IsFourOfAKind _ -> true
    | _ -> false

[ isStraight FiveCards.straightCards
  not <| isStraight FiveCards.testData
  isStraight FiveCards.wheelStraightCards

  FiveCards.straightCards |> function FiveCards.FiveCards cards -> not <| isBroadway cards
  FiveCards.broadwayCards |> function FiveCards.FiveCards cards -> isBroadway cards

  FiveCards.testData |> function FiveCards.FiveCards cards -> not <| isFlush cards
  FiveCards.suitedCards |> function FiveCards.FiveCards cards -> isFlush cards

  FiveCards.testData |> isFullHouse |> not
  FiveCards.fullHouse |> isFullHouse

  FiveCards.testData |> isThreeOfAKind |> not
  FiveCards.threeOfAKind |> isThreeOfAKind

  FiveCards.fullHouse |> isTwoPair |> not
  FiveCards.twoPair |> isTwoPair

  FiveCards.twoPair |> isPair |> not
  FiveCards.pair |> isPair

  FiveCards.pair |> isHighCard |> not
  FiveCards.testData |> isHighCard

  FiveCards.testData |> isFourOfAKind |> not
  FiveCards.fourOfAKind |> isFourOfAKind ]

//let isSuited cards =
//    match cards with
//    | Suited _ -> true
//    | _ -> false
//
//

//let solver: PokerHandSolver =
//    fun cards ->
//        match cards with
//        | Broadway _ & Suited _ -> RoyalFlush
//        | Straight _ & Suited _ -> StraightFlush
