{-

Garrett Hill
CMSC488B
Spring 2022

------------------ DISCLAIMERS --------------------

This demonstration heavily utilizes code from the following repository:
https://github.com/ivanjermakov/wordle
Please refer to src/Engine.hs in particular.

This example uses suboptimal algorithms that will slow down significantly
for larger data sets. For example, only the 10k and official Wordle word sets are able to run on my machine without hanging.
It serves more as a proof of concept that is readable and PROVABLY [in]correct
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
w_pool        = lines $ get_pool "reference_dict/official.txt"

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

{- Default Wordle and Veil for testing -}
def_w :: Wordle
def_w = (W [] w_pool)

def_v :: Veil
def_v = (V def_w "cabal" 5)

{-
Generating "complex" arbitrary wordles is impossible due to not being
able to make any guesses.
-}
instance Arbitrary Wordle where
  arbitrary = return def_w
  shrink (W (g:gs) wp) = []

{-
All we know how to test until we define more complex actions
-}
prop_wValid :: Wordle -> Bool
prop_wValid w@(W gs wp) = length gs <= 5 && length wp <= length w_pool

{-
Veils will be the primary testing datatype as they can actually take turns.
Note that this example intentionally defines arbitrary veils as randomly
guessing for each turn.
This random incompetance means that we should (usually) not have enough
guesses left to be able to solve random Veils
-}
instance Arbitrary Veil where
  arbitrary = do
      init@(V w k t) <- V <$> (return def_w) <*> elements w_pool <*> return 5
      turns <- elements [0 .. 5]
      arb_turns init turns
  shrink _ = []

{-
For a given veil, executes the number of turns by randomly guessing from the
remaining word pool
-}
arb_turns :: Veil -> Int -> Gen Veil
arb_turns v@(V w@(W gs wp) k t) i
  | solveable v = return v
  | i > 0 = do
      new_guess <- elements wp
      arb_turns (veilTurn new_guess v) (i-1)
  | otherwise = return v

{-
For a given veil, returns whether that veil satisfies a list of validity
conditions
-}
prop_vValid :: Veil -> Bool
prop_vValid v@(V w k t) = and val_cons
  where
    val_cons = (length k == 5) : (t <= 5) : (prop_wValid w) : []


---------------- FUNCTIONS ------------------

{- Pass a string to a veil to guess, and returned the updated veil -}
veilTurn :: String -> Veil -> Veil
veilTurn s v@(V w@(W gs wp) k t)
  | solveable v = v
  | t >= 1 = let new_g = guess s k
                 new_gs = new_g : gs
             in
      (V (W new_gs (trimPool new_gs wp)) k (t-1))
  | otherwise = v -- no more turns, do nothing

{- NOTE: COMES VERBATIM FROM GITHUB CITED ABOVE
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
-}
v_uv' :: Veil -> Int
v_uv' v@(V w@(W gs wp) k t) = (w_uv w) `div` t

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

{-
For a given veil, produces the list of all unique veils with one additional
valid word guessed
-}
v_nb :: Veil -> [Veil]
v_nb v@(V w@(W gs wp) k t) = let new_ws = w_nb w k in
  [(V new_w k (t-1)) | new_w <- new_ws]  

{-
Experimental neighborhood algorithm designed to reduce the guesses to only those
that eliminate a significant portion of results
-}
v_nb' :: Veil -> [Veil]
v_nb' v@(V w@(W gs wp) k t) =
  let new_pool = trimPool gs wp
      new_gs   = [guess g k | g <- new_pool]
      new_ws   = [(W (new_g : gs) new_pool) | new_g <- new_gs]
      new_vs   = [(V new_w k (t-1)) | new_w <- new_ws]
  in
    filter (\new_v -> v_uv' new_v <= v_uv' v) new_vs

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
-}

prop_solveableIn :: IO ()
prop_solveableIn = quickCheck (counterexample "solveable" $ solveableIn 5)
-- prop_solveableIn takes (101.87 secs, 44,426,540,840 bytes)

tqc_solveableIn :: IO ()
tqc_solveableIn = minUVUnder v_uv v_nb acceptableRisk 5
-- always below 0.2 seconds and 40,000,000 bytes

tqc_solveableIn' :: IO ()
tqc_solveableIn' = minUVUnder v_uv' v_nb' acceptableRisk 5
-- always below 0.2 seconds and 40,000,000 bytes
