{-

Garrett Hill
CMSC488B
Spring 2022

-}

{-# LANGUAGE TemplateHaskell   #-}

module Examples where

import Data.List

import TargetedPBT
import Test.QuickCheck

import Control.Monad
import Control.Lens


------------- DEFINITIONS ----------------

data Wordle = Wordle {
    _next_g      :: [Char],        -- what we want to guess next
    _greens      :: [(Char, Int)], -- chars we know the position of
    _yellows     :: [Char],        -- chars we don't know the place of
    _greys       :: [Char]         -- chars not in the list
} deriving (Eq, Show)

data Game = Game {
    _wordle      :: Wordle, -- the wordle trying to guess the key
    _key         :: [Char], -- the key the wordle is trying to guess
    _turns       :: Int     -- how many turns are left
} deriving (Eq, Show)

wordle_size :: Int
wordle_size = 5

{-
What utility value function should we use for this example? For these wordles, I will define a reward function
that values the length of green, the length of yellow, and the length of the grey lists. Mathematically:
UV(x) = 100 * x.grn.length + 10 * x.yel.length + 1 * x.gry.length
-}

(grn_weight, yel_weight, gry_weight) = (100,10,1)

w_uv :: Wordle -> Int
w_uv (Wordle c_g grn yel gry) = (grn_weight * length grn) + (yel_weight * length yel) + (gry_weight * length gry)

{-
What neighborhood function should we use for this example? For these wordles, I will define a neighborhood function that produces all guesses that are one character off from the current guess of the input wordle. These neighboring Wordles have not actually guessed the new string yet; it is their *upcoming* guess.
-}

w_nb :: Wordle -> [Wordle]
w_nb w@(Wordle c_g grn yel gry) = [(Wordle s grn yel gry) | s <- string_nb c_g]

string_nb :: String -> [String]
string_nb s =
  -- replace all chars with list of all alternate chars
  let rep_w = map (\c1 -> (c1,[c2 | c2 <- ['a' .. 'z'], not (c1 == c2)])) s in
    [l | (c,l) <- rep_w] -- TODO: NEEDS FIXING

-- Generate a new wordle Guess based on grn yel and gry
-- TODO: IMPLEMENT NON TRIVIAL SOLUTION
new_guess :: String -> [(Char, Int)] -> String -> String -> String
new_guess c_g grn yel gry =
  let pool = ['a' .. 'z']
      s = [p | p <- pool, not (elem p gry)]
  in
    s
  

-- Execute the current guess for the game
guess :: Game -> Game
guess (Game (Wordle c_g grn yel gry) k t) | t >= 0 =
  let (new_grn,new_yel,new_gry) = (comp c_g k)
      ng = new_guess c_g new_grn new_yel new_gry
  in
    (Game (Wordle ng (grn ++ new_grn) (yel ++ new_yel) (gry ++ new_gry)) k (t-1))
  | otherwise = error "no turns left!"

-- Gets a list of new entries for lists to add to the Wordle
comp :: String -> String -> ([(Char, Int)], String, String)
comp c_g k = (ud_grn c_g k 0, ud_yel c_g k, ud_gry c_g k)

ud_grn :: String -> String -> Int -> [(Char, Int)]
ud_grn (gh:gt) (kh:kt) i
  | gh == kh = (gh,i) : ud_grn gt kt (i+1)
  | not (gh == kh) = ud_grn gt kt (i+1)
ud_grn _ _ _ = []

ud_yel :: String -> String -> String
ud_yel c_g k = [c | c <- c_g, elem c k]

ud_gry :: String -> String -> String
ud_gry c_g k = [c | c <- c_g, not (elem c k)]


makeLenses ''Wordle
makeLenses ''Game





---------- TESTING ------------
-- A default Wordle to test with
def_w :: Wordle
def_w = Wordle "haskl" [] [] []

-- A default Game to test with
def_g :: Game
def_g = Game def_w "cabal" 5

gen_str :: Int -> Gen [Char]
gen_str i = vectorOf i (choose ('a', 'z'))

instance Arbitrary Wordle where
  arbitrary = Wordle <$> (gen_str wordle_size) <*> (return []) <*> (return [])  <*> (return [])
  shrink (Wordle c_g grn yel gry) = []

instance Arbitrary Game where
  arbitrary = Game <$> arbitrary <*> (gen_str wordle_size) <*> (return 5) -- change 5 to arbitrary non neg int
  shrink (Game w k t) = []

-- Check that all arbitrary wordle are correctly formatted
prop_wordleValid :: Wordle -> Bool
prop_wordleValid (Wordle c_g grn yel gry) = (c_g_valid c_g) && (grn_valid grn) && (yel_valid yel) && (gry_valid gry)

-- Check that all arbitrary games are correctly formatted
prop_gameValid :: Game -> Bool
prop_gameValid (Game w k t) = (prop_wordleValid w) && (k_valid k) && (t_valid t)

-- Check that a given string is a correctly formatted wordle guess
c_g_valid :: [Char] -> Bool
c_g_valid c_g = length c_g == wordle_size

-- Checks that a given list is a correctly formatted list of known character positions
grn_valid :: [(Char, Int)] -> Bool
grn_valid grn = length grn <= wordle_size

-- Checks that a given list is a correctly formatted list of known characters without known position
yel_valid :: [Char] -> Bool
yel_valid yel = length yel <= wordle_size

-- Checks that a given list is a correctly formatted list of characters known not to be in the key
gry_valid :: [Char] -> Bool
gry_valid gry = length gry <= 26 - wordle_size

-- Checks that a given list is a correctly formatted game key
k_valid :: [Char] -> Bool
k_valid k = length k == wordle_size

-- Checks that the number of turns left is valid
t_valid :: Int -> Bool
t_valid t = t >= 0




-- The best score is all green, getting green weight * wordle_size points, and the min is 0 (at the start of the game)
prop_uvValid :: Wordle -> Bool
prop_uvValid w = let score = w_uv w in score <= (grn_weight * wordle_size) && score >= 0

prop_nbsValid :: Wordle -> Bool
prop_nbsValid w = all prop_wordleValid (w_nb w)

-- Check that guessing always produces a valid gamestate
prop_guessValid :: Game -> Bool
prop_guessValid g = prop_gameValid (guess g)

prop_newGuessValid :: Wordle -> String -> Bool
prop_newGuessValid (Wordle c_g grn yel gry) s | length s == wordle_size = prop_wordleValid (Wordle s grn yel gry)
prop_newGuessValid w s = True

{-
Haskell IFC:
https://arxiv.org/pdf/1409.0393.pdf
https://github.com/QuickChick/TestingNoninterference

roadmap:
implement typeclass to demonstrate properties of some object's state
- need to include utility function and neighborhood function
- implement arbitrary so that you can generate instances of the objects
- for every test that quickcheck generates an arbitrary instance for, gradient[As/Des]scent to check nearby points for higher likelhihood to fail based on predefined utility and heighborhood functions
-}


testAll :: IO ()
testAll = do
  quickCheck prop_wordleValid
  quickCheck prop_gameValid
  quickCheck prop_uvValid
  quickCheck prop_nbsValid
  quickCheck prop_guessValid
  quickCheck prop_newGuessValid
