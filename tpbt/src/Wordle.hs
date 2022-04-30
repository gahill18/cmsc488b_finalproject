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

{- Default Wordle -}
def_w :: Wordle
def_w = (W [] w_pool)

{-
Generating "complex" arbitrary wordles is impossible due to not being
able to make any guesses
-}
instance Arbitrary Wordle where
  arbitrary = return def_w
  shrink (W (g:gs) wp) = []

prop_wValid :: Wordle -> Bool
prop_wValid w@(W gs wp) = length gs <= 5 

{-
Veils will be the primary testing datatype as they can actually take
turns
-}
instance Arbitrary Veil where
  arbitrary = do
      init@(V w k t) <- V <$> (return def_w) <*> elements w_pool <*> return 5
      turns <- elements [0 .. 5]
      -- get list of non-evaled functions that would pick from w_pool
      arb_turns init turns
  shrink _ = []

arb_turns :: Veil -> Int -> Gen Veil
arb_turns v@(V w@(W gs wp) k t) i
  | solveable v = return v
  | i > 0 = do
      new_guess <- elements wp
      arb_turns (veilTurn new_guess v) (i-1)
  | otherwise = return v

prop_vValid :: Veil -> Bool
prop_vValid v@(V w k t) = (length k == 5) && (t <= 5) && prop_wValid w


---------------- FUNCTIONS ------------------

veilTurns :: Int -> [String] -> Veil -> Veil
veilTurns i (s:ss) v@(V w@(W gs wp) k t)
  | solveable v = v
  | i == 1 = veilTurn s v
  | i >= 2 = veilTurns (i-1) ss (veilTurn s v)
veilTurns i ss v = v

{- Pass a string to a veil to guess, and returned the updated veil -}
veilTurn :: String -> Veil -> Veil
veilTurn s v@(V w@(W gs wp) k t)
  | solveable v = v
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
          | elem g tWord = WrongSpot
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

{- Checks if a given string violates any of the guesses -}
violatesList :: String -> [Guess] -> Bool
violatesList s [] = False
violatesList s (g:gs) = violates s s g || violatesList s gs

{-
Checks if a given string violates a single guess
-}
violates :: String -> [Char] -> Guess -> Bool
violates whole_s rem_s@(c:cs) ((gc,gt):gs) = case gt of
    Default -> True
    NotInWord -> elem gc whole_s || violates whole_s cs gs
    WrongSpot -> (not $ elem c whole_s) || c == gc || violates whole_s cs gs
    CorrectSpot -> not $ c == gc || violates whole_s cs gs
violates _       []        []          = False
violates _       _         _           = True


{-
Any wordle should not have any words in the pool that would violate past
guesses
The actual key should also always be in the pool
-}
prop_poolValid :: Veil -> Property
prop_poolValid v@(V w@(W gs wp) k t) =
  counterexample "invalid word in pool" $
  (not $ any (\word -> violatesList word gs) wp) &&
  (elem k wp)

------------------ TARGETED PBT --------------------

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
The utility value of a veil is just the utility value of the contained
Wordle
-}
v_uv :: Veil -> Int
v_uv v@(V w@(W gs wp) k t) = w_uv w

{-
returns if a given veil is solveable in i turns
-}
solveableIn :: Int -> Veil -> Bool
solveableIn i v@(V w@(W gs wp) k t)
  | i > 1  = any (solveableIn (i-1)) (v_nb v)
  | otherwise = solveable v

{-
You have successfully narrowed down the possible words to a defined
risk range
-}
solveable :: Veil -> Bool
solveable v@(V w@(W gs wp) k t) = length wp <= acceptableRisk

{-
how small our pool needs to be for us to consider randomly guessing as a valid
solution to the Wordle
for acceptableRisk = n, % chance of guessing correctly is (1/n) * 100
    acceptableRisk = 5, % chance of guessing correctly is (1/5) * 100 = 20%
-}
acceptableRisk = 5
  

{-
Compare the speed difference between quickCheckig (solveableIn 5) and
minUVUnder acceptableRisk

Both should produce counterexamples for the same property, but tqc should be
significantly faster

On my machine,
tqc_solveable takes (0.14 secs, 25,552,648 bytes) to find a counterexample
prop_solveableIn takes (101.87 secs, 44,426,540,840 bytes)
-}

prop_solveableIn :: IO ()
prop_solveableIn = quickCheck (counterexample "solveable" $ solveableIn 5)

tqc_solveableIn :: IO ()
tqc_solveableIn = minUVUnder v_uv v_nb acceptableRisk 5
