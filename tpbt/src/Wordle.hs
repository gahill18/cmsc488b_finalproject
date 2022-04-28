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

import TargetedPBT

import Data.List
import Test.QuickCheck

import System.IO.Unsafe
import Control.Monad



---------------- DEFINITIONS ------------------

{-
These three intial datatypes are pulled from https://github.com/ivanjermakov/wordle
-}
data GuessType = Default | NotInWord | WrongSpot | CorrectSpot
  deriving (Show, Read, Eq, Ord)

type Guess = [(Char, GuessType)]

data GameStatus = Win | Loss | Ongoing
  deriving (Show, Read, Eq, Ord)

{- The official list of Wordle keys -}
get_pool path = unsafePerformIO . readFile $ path
w_pool = lines $ get_pool "reference_dict/official.txt"

{- Datatype to store past guesses and possible words left to guess -}
data Wordle = W {
    guesses   :: [Guess]
  , word_pool :: [String]
  } deriving (Show, Read, Eq, Ord)

{- Datatype to hide the key from the wordle -}
data Veil = V {
    wordle    :: Wordle
  , key       :: String
  , turns     :: Int
  } deriving (Show, Read, Eq, Ord)

{- Default Testing Wordle -}
def_w :: Wordle
def_w = (W [] w_pool)

{- Default Testing Veil -}
def_v :: Veil
def_v = (V def_w "cabal" 5)

instance Arbitrary Wordle where
  arbitrary = W <$> (return [])  <*> (return w_pool)
  shrink (W (g:gs) wp) = [(W gs wp)]
  shrink w = []

instance Arbitrary Veil where
  arbitrary = V <$> arbitrary <*> elements w_pool <*> return 5
  shrink (V w k t) = []


---------------- FUNCTIONS ------------------

veilTurns :: Int -> [String] -> Veil -> Veil
veilTurns i (s:ss) v@(V w@(W gs wp) k t)
  | i == 1 = veilTurn s v
  | i >= 2 = veilTurns (i-1) ss (veilTurn s v)
veilTurns i ss v = v

{- Pass a string to a veil to guess, and returned the updated veil -}
veilTurn :: String -> Veil -> Veil
veilTurn s v@(V w@(W gs wp) k t)
  | t >= 1 = let new_g = guess s k
                 new_gs = new_g : gs
             in
      (V (W new_gs (trimPool new_gs wp)) k (t-1))
  | otherwise = v -- no more turns, do nothing

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
trimPool :: [Guess] -> [String] -> [String]
trimPool gs wp = [s | s <- wp, not (s `violatesList` gs)]

violatesList :: String -> [Guess] -> Bool
violatesList s [] = False
violatesList s (g:gs) = violates s s g || violatesList s gs

violates :: String -> [Char] -> Guess -> Bool
violates whole_s s@(c:cs) ((gc,gt):gs) = case gt of
    Default -> violates whole_s cs gs
    NotInWord -> elem gc whole_s || violates whole_s cs gs
    WrongSpot -> (not $ elem c cs) || c == gc || violates whole_s cs gs
    CorrectSpot -> not $ c == gc || violates whole_s cs gs
violates whole_s []        []          = False
violates _       _         _           = True


------------------ TARGETED PBT --------------------

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
  let new_pool = trimPool gs wp
      new_gs   = [guess gWord tWord | gWord <- new_pool]
  in
    [(W (new_g : gs) new_pool) | new_g <- new_gs]

v_nb :: Veil -> [Veil]
v_nb v@(V w@(W gs wp) k t) = let new_ws = w_nb w k in
  [(V new_w k (t-1)) | new_w <- new_ws]

{-
Produces a utility value by comparing the length of the wordle's pool
to the length of the total word pool

This value should be minimized to find the optimal series of guesses
-}
w_uv :: Wordle -> Int
w_uv (W gs wp) = (length wp) ^ 2

{-
-}
v_uv :: Veil -> Int
v_uv v@(V w@(W gs wp) k t) = w_uv w

{-
For all x such that x is a member of w_pool,
there exists a series of five or less guesses to
arrive at that answer
-}
solveable :: Veil -> Bool
solveable v@(V w@(W gs wp) k t) =
  length wp == 1 ||
  any solveable (v_nb v)
  
