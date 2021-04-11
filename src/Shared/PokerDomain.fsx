// 5 cards -> Poker hand

open System
open Microsoft.FSharp.Reflection

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
let queenOfDiamonds = { Suit = Diamonds ; Value = Queen }
let twoOfSpades = { Suit = Spades; Value = Two }
let threeOfSpades = { Suit = Spades; Value = Three }
let sixOfSpades = { Suit = Spades; Value = Six }
let kingOfSpades = { Suit = Spades; Value = King }
let nineOfSpades = { Suit = Spades; Value = Nine }
let tenOfSpades = { Suit = Spades; Value = Ten }
let kindOfDiamonds = { Suit = Diamonds; Value = King }

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
    | StraightFlush of SortedSuitedFiveCards
    | RoyalFlush of SortedSuitedFiveCards

type PokerHandSolver = FiveCards.FiveCards -> PokerHand

let (|Suited|_|) (FiveCards.FiveCards cards) =
    let suits =
        cards
        |> List.map (fun card -> card.Suit)

    let allSuited =
        suits
        |> List.forall ((=) (suits |> List.head))

    if allSuited then
        Suited Some cards
    else
        Suited None

let (|Sorted|) cards =
    Sorted List.sortDescending cards

let isStraight cards =
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

let (|Straight|_|) (Sorted cards) =
    if isStraight cards then
        Straight Some cards
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

let (|Broadway|_|) (Sorted (cards: Card list)) =
    if isBroadway cards then
        Broadway Some ()
    else
        None

[ FiveCards.testData |> function FiveCards.FiveCards cards -> not <| isStraight cards

  FiveCards.straightCards |> function FiveCards.FiveCards cards -> isStraight cards
  FiveCards.straightCards |> function FiveCards.FiveCards cards -> not <| isBroadway cards
  FiveCards.broadwayCards |> function FiveCards.FiveCards cards -> isBroadway cards

  FiveCards.wheelStraightCards |> function FiveCards.FiveCards cards -> isStraight cards ]



//let isSuited cards =
//    match cards with
//    | Suited _ -> true
//    | _ -> false
//
//isSuited FiveCards.testData
//isSuited FiveCards.suitedCards

//let solver: PokerHandSolver =
//    fun cards ->
