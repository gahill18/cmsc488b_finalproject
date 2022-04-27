{-

Garrett Hill
CMSC488B
Spring 2022

------------------ DISCLAIMERS --------------------
This example uses suboptimal algorithms that would likely slow down
significantly for larger data sets.
It serves more as a proof of concept that is readable and PROVABLY correct
within user defined margins.
Caveat emptor.

-}

module Wordle where

import Data.List
import Test.QuickCheck

{-
These three intial datatypes are pulled from https://github.com/ivanjermakov/wordle
-}
data GuessType = Default | NotInWord | WrongSpot | CorrectSpot
  deriving (Show, Read, Eq, Ord)

type Guess = [(Char, GuessType)]

data GameStatus = Win | Loss | Ongoing
  deriving (Show, Read, Eq, Ord)

{- TODO: EXPAND TO FULL POOL -}
def_pool :: [String]
def_pool =
  let subs = subsequences ['a' .. 'z']
      filtered_subs = [s | s <- subs, (length s) == 5]
  in
    take 15 filtered_subs

{- Datatype to store past guesses and possible words left to guess -}
data Wordle = W {
    guesses   :: [Guess]
  , word_pool :: [String]
  } deriving (Show, Eq)

{- Datatype to hide the key from the wordle -}
data Veil = V {
    wordle    :: Wordle
  , key       :: String
  } deriving (Show, Eq)

instance Arbitrary Wordle where
  arbitrary = W <$> (return [])  <*> (return def_pool)
  shrink (W (g:gs) wp) = [(W gs wp)]
  shrink w = []

instance Arbitrary Veil where
  arbitrary = V <$> arbitrary <*> (vectorOf 5 (choose ('a', 'z')))
  shrink (V w k) = []

{-
Takes a guess and key, and produces the necessary feedback to the user.
-}
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

{-
For a given list of guesses and a word pool, remove any words from the
word pool that violate the information contained in the guesses
-}
trim_pool :: [Guess] -> [String] -> [String]
trim_pool gs wp = [s | s <- wp, not (s `violatesList` gs)]

violatesList :: String -> [Guess] -> Bool
violatesList s [] = False
violatesList s (g:gs) = violates s g || violatesList s gs

violates :: String -> Guess -> Bool
violates s g = False

{-
Any wordle should not have any words in the pool that would violate past
guesses
-}
prop_poolValid :: Wordle -> Property
prop_poolValid w@(W gs wp) = counterexample "poolValid" $
  not $ or $ map (\s -> violatesList s gs) wp

{-
For a given wordle, produces the list of all Wordles that have had one
additional word guessed. The additional word is determined by the values
in the remaining word pool.
-}
w_nb :: Wordle -> String -> [Wordle]
w_nb w@(W gs wp) tWord =
  let new_pool = trim_pool gs wp
      new_gs   = [guess gWord tWord | gWord <- new_pool]
  in
    [(W (new_g : gs) new_pool) | new_g <- new_gs]

{-
Produces a utility value by comparing the length of the wordle's pool
to the length of the total word pool
-}
w_uv :: Wordle -> Int
w_uv (W gs wp) = length def_pool - length wp
