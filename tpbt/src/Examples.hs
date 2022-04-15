{-

Garrett Hill
CMSC488B
Spring 2022

-}

{-# LANGUAGE TemplateHaskell   #-}

module Examples where

import TargetedPBT
import Test.QuickCheck (
  Arbitrary,
  Property,
  arbitrary,
  choose,
  counterexample,
  elements,
  listOf,
  quickCheck,
  sample,
  vectorOf
  )
import qualified Test.QuickCheck as QC
import Control.Lens

data Wordle = Wordle {
    _curr_g      :: [Char],        -- what we want to guess next
    _greens      :: [(Char, Int)], -- items we know the position of
    _yellows     :: [Char],        -- items we don't know the place of
    _greys       :: [Char],        -- items not in the list
    _turns_remn  :: Int         -- turns remaining
} deriving (Eq, Show)

data Game = Game {
    _wordle      :: Wordle, -- the wordle trying to guess the key
    _key         :: [Char]  -- the key the wordle is trying to guess
}

makeLenses ''Wordle
makeLenses ''Game

wordle_size = 5

instance Arbitrary Wordle where
  arbitrary = QC.elements [Wordle {
     _curr_g = gen_str wordle_size   -- change to random str of len wordle_size
    ,_greens = []        :: [(Char, Int)]  -- nothing known
    ,_yellows = []       :: [Char]         -- ^^^^^^^^^^^^^
    ,_greys = []         :: [Char]         -- ^^^^^^^^^^^^^
    ,_turns_remn = 5                 -- this may need to change later
    }]
  shrink = undefined

gen_str :: Int -> String
gen_str i = ""


prop_validWordle :: Wordle -> Property
prop_validWordle (Wordle c_g grn yel gry trn)
  | not (c_g_valid c_g) = counterexample ("c_g invalid: " ++ c_g) False
  | not (grn_valid grn) = counterexample ("grn invalid: " ++ show (grn)) False
  | not (yel_valid yel) = counterexample ("yel invalid: " ++ yel) False
  | not (gry_valid gry) = counterexample ("gry invalid: " ++ gry) False
  | not (trn_valid trn) = counterexample ("trn invalid: " ++ (show trn)) False
  | otherwise = counterexample "" True

c_g_valid :: [Char] -> Bool
c_g_valid c_g = length c_g == wordle_size

grn_valid :: [(Char, Int)] -> Bool
grn_valid grn = length grn <= wordle_size

yel_valid :: [Char] -> Bool
yel_valid yel = length yel <= wordle_size

gry_valid :: [Char] -> Bool
gry_valid gry = length gry <= 26 - wordle_size

trn_valid :: Int -> Bool
trn_valid trn = True



main :: IO ()
main = do
  print "hi mom"

{-
Haskell IFC:
https://arxiv.org/pdf/1409.0393.pdf
https://github.com/QuickChick/TestingNoninterference

potential typeclass: wordle representation
fields:
- green, yellow, and grey letters in lists
- current guess
uv:
- option 1: how much information is gained in a guess
- option 2: how much information is already obtained from past guesses
neighborhood:
- all words with one letter changed (that isnt green or yellow)
arbitrary:
- initialized as random word with no green, yellow, or greys


roadmap:
implement typeclass to demonstrate properties of some object's state
- need to include utility function and neighborhood function
- implement arbitrary so that you can generate instances of the objects
- for every test that quickcheck generates an arbitrary instance for, gradient[As/Des]scent to check nearby points for higher likelhihood to fail based on predefined utility and heighborhood functions
-}


