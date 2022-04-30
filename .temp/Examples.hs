{-

Garrett Hill
CMSC488B
Spring 2022

-}

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}


module Examples where

import TargetedPBT
import QuickCheck
import Control.Lens

data (Wordle g) = MkResult {
    _curr_g      :: [g],
    _greens      :: [(g, Int)], -- items we know the position of
    _yellows     :: [g],        -- items we don't know the place of
    _greys       :: [g],        -- items not in the list
    _turns_remn  :: Int         -- turns remaining
}

data (Game g) = MkHistory {
    _wordle      :: (Wordle g),
    _key         :: [g]
}

makeLenses ''Wordle
makeLenses ''Game

instance Arbitrary Wordle where
  arbitrary = Turn {
    _guess = (arbitrary :: [Char]),
    _green = [],
    _yellow = [],
    _grey = []
    }



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


