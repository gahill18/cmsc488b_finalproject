{-

Garrett Hill
CMSC488B
Spring 2022

A significant portions of this Wordle representation was taken directly from the following haskell project:
  https://github.com/ivanjermakov/wordle
All property testing is my own



-}

module Wordle where

import Data.List
import Data.List.Utils

import TargetedPBT
import Test.QuickCheck

import Control.Monad

------------- DEFINITIONS/ENGINE ----------------


data GuessType = Default | NotInWord | WrongSpot | CorrectSpot
  deriving (Show, Read, Eq, Ord)

type Guess = [(Char, GuessType)]

data GameStatus = Win | Loss | Ongoing
  deriving (Show, Read, Eq, Ord)

data Wordle = W {
    pst_g       :: [Guess]  -- Past guesses
  , pos_g       :: [String] -- All possible guesses
} deriving (Eq, Show, Ord)

data Engine = E {
    t         :: Int     -- Turns left
  , k         :: String  -- Key to the wordle
} deriving (Eq, Show, Ord)

-- The player gives its guess to the engine so the engine can tell the player
-- how well it did (the player ideally never sees k)
askEngine :: Engine -> String -> Guess
askEngine (E t k) g | t > 0 = guess g k
askEngine _ _ = []

-- Determine what information to give back to the player
guess :: String -> String -> Guess
guess gWord tWord = foldl f [] $ zip gWord tWord
  where
    f :: Guess -> (Char, Char) -> Guess
    f acc (g, t) = acc ++ [(g, guessType)]
      where
        guessType
          | g == t = CorrectSpot
          | targetTotal g - correctTotal g - wrongBefore g > 0 = WrongSpot
          | otherwise = NotInWord
        wrongBefore c = length . filter (== c) . map fst . filter ((== WrongSpot) . snd) $ acc
        targetTotal c = length . filter (== c) $ tWord
        correctTotal c = length . filter (\(a, b) -> a == c && b == c) $ zip gWord tWord

-- Check that our guessing code satisfies the properties we want it to
prop_correctSpotValid :: String -> Property
prop_correctSpotsValid s
  | length s > 0 = let cg = guess s s in counterexample "s on s is always correct" $
    any (\(a,b) -> case b of
            CorrectSpot -> True
            _ -> False
        ) cg
  | otherwise = counterexample "" True

prop_WrongSpotValid :: String -> Property
prop_wrongSpotsValid s = undefined

prop_NotInWordValid :: String -> Property
prop_NotInWordValid s = undefined
