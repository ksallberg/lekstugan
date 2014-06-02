-- INTRODUKTION TILL FUNKTIONELL PROGRAMMERING
-- LABORATION 2 - Black Jack
-- Joel Ödlund, Kristian Sällberg

-- Note:  QuickCheck properties at the end.

module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck
import System.Random

--  Task 3.2
--
--  hand2 = Some (Card (Numeric 2) Hearts)
--               (Some (Card Jack Spades ) Empty)
--
--  size hand2
--  = size Some (Card (Numeric 2) Hearts)
--              (Some (Card Jack Spades) Empty)
--  = 1 + size (Some (Card Jack Spades) Empty)
--  = 1 + 1 + size Empty
--  = 1 + 1 + 0
--  = 2

-- Returns the value of a card
valueCard :: Card -> Integer
valueCard (Card Jack        _) = 10
valueCard (Card Queen       _) = 10
valueCard (Card King        _) = 10
valueCard (Card Ace         _) = 11
valueCard (Card (Numeric a) _) = a

-- Returns how many aces a hand contains
numberOfAces :: Hand -> Integer
numberOfAces Empty                    = 0
numberOfAces (Some (Card Ace _) hand) = 1 + (numberOfAces hand)
numberOfAces (Some card hand)         = numberOfAces hand

-- Returns an empty hand
empty :: Hand
empty = Empty

-- Returns the value of a hand deducted by
-- 10 for each Ace if the combined value > 21.
value :: Hand -> Integer
value hand | (value' hand) > 21 && (numberOfAces hand) >= 1
              = (value' hand) - ((numberOfAces hand) * 10)
           | otherwise = (value' hand)

-- Returns the combined value of all cards in a hand.
value' :: Hand -> Integer
value' Empty = 0
value' (Some card hand) = (valueCard card) + value' hand

-- Is the player bust?
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Compares the guest and the bank and returns the winner.
winner :: Hand -> Hand -> Player
winner pHand bHand | gameOver pHand = Bank
                   | gameOver bHand = Guest
                   | value pHand > value bHand = Guest
                   | otherwise = Bank

-- Adds a hand to another hand
(<+) :: Hand -> Hand -> Hand
Empty            <+ hand2 = hand2
(Some card hand) <+ hand2 = Some card (hand <+ hand2)

-- Returns a hand of all cards in a given suit
fullSuit :: Suit -> Hand
fullSuit suit =
            (Some (Card Ace   suit) Empty)
         <+ (Some (Card King  suit) Empty)
         <+ (Some (Card Queen suit) Empty)
         <+ (Some (Card Jack  suit) Empty)
         <+ numerics 10 suit

-- returns numerics of a suit, from n to 2
numerics :: Integer -> Suit -> Hand
numerics 1 _    = Empty
numerics n suit = (Some (Card (Numeric n) suit) Empty)
                        <+ (numerics (n-1) suit)

-- Returns a full deck of cards
fullDeck :: Hand
fullDeck = (fullSuit Hearts)
        <+ (fullSuit Diamonds)
        <+ (fullSuit Clubs)
        <+ (fullSuit Spades)

-- Removes the top card from a hand and adds it to another hand.
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _               = error "draw: The deck is empty"
draw (Some card rest) hand = (rest,  (Some card Empty) <+ hand)

-- Takes a deck from the wrapper, and fills an empty hand with
-- as many cards required to get to value = 16 or larger for
-- the cards.
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- Recursively fills bankHand with cards until the value of
-- all cards is >= 16. Auxiliary function for playBank.
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand
   | value bankHand < 16  = playBank' deck' bankHand'
   | otherwise = bankHand
      where (deck', bankHand') = draw deck bankHand

-- Shuffles a hand,  based on a StdGen
shuffle :: StdGen -> Hand -> Hand
shuffle _ Empty = Empty
shuffle generator hand 
   = (takeCard hand n) <+ (shuffle g (removeCard hand n))
      where (n, g) = randomR (1, size hand) generator

-- Returns a card on place n from a given hand.
-- Card is returned as a hand with 1 card.
-- Aux function to shuffle
takeCard :: Hand -> Integer -> Hand
takeCard (Some card hand) 1 = Some card Empty
takeCard (Some card hand) n = takeCard hand (n-1)

-- Takes a hand and returns the same hand,  minus the
-- card at position n Aux function to shuffle
removeCard :: Hand -> Integer -> Hand
removeCard (Some card hand) 1 = hand
removeCard (Some card hand) n =
   (Some card Empty) <+ removeCard hand (n-1)

-- Implementation, provides a list
-- of functions for Wrapper to use
implementation = Interface {
   iEmpty     = empty,
   iFullDeck  = fullDeck,
   iValue     = value,
   iGameOver  = gameOver,
   iWinner    = winner,
   iDraw      = draw,
   iPlayBank  = playBank,
   iShuffle   = shuffle
}

-- Main function
main :: IO()
main = runGame implementation

-----------------------
--  Test properties  --
-----------------------

-- TEST +>
-- Tests for associativity of +>
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- Tests if the size of 2 combined hands is the sum of
-- the sizes of the individual hands.
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size (p1 <+ p2) == size p1 + size p2

-- TEST shuffle
-- Check if a card is in the hand, AND in the shuffled hand
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards gen card hand =
    card `belongsTo` hand == card `belongsTo` (shuffle gen hand)

-- Checks if a card is part of a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty  = False
c `belongsTo` ( Some c' h ) = c == c' || c `belongsTo` h

-- Checks if shuffled hand has the same size as original hand
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle gen hand = size hand  == size (shuffle gen hand)