{-

Garrett Hill
CMSC488B
Spring 2022

-}

module Wordle where

import Data.List
import Data.List.Utils

import TargetedPBT
import Test.QuickCheck

import Control.Monad

------------- DEFINITIONS/ENGINE ----------------

data Wordle = Wordle {
    _c_g         :: String,        -- what we are about to guess
    _grn         :: [(Char, Int)], -- chars we know the position of
    _yel         :: [(Char, Int)], -- chars we know exist outside of given positions
    _gry         :: [Char],        -- chars not in the key
    _pool        :: [Char]         -- chars left to use
} deriving (Eq, Show)

data Game = Game {
    _wordle      :: Wordle, -- the wordle trying to guess the key
    _key         :: String, -- the key the wordle is trying to guess
    _turns       :: Int     -- how many turns are left
} deriving (Eq, Show)

wordle_size :: Int
wordle_size = 5

game_turns :: Game -> Int -> Game
game_turns g i | i > 1 = game_turns (game_turn g) (i-1)
               | i == 1 = game_turn g
               | otherwise = error "Invalid number of game_turns!"

-- Steps through one turn of the game
game_turn :: Game -> Game
game_turn g = new_guess (guess g)

-- Pick a new guess based on current known info
-- DOES NOT decrease turns
new_guess :: Game -> Game
new_guess g@(Game w@(Wordle c_g grn yel gry pool) k t)
  | length grn == wordle_size = error "Wordle already solved!"
  | t > 1 = (Game (non_final_guess w) k t)
  | t == 1 = (Game (final_guess w) k 0)
  | otherwise = g

-- Pick a new guess that would be acceptable for a nonfinal guess
non_final_guess :: Wordle -> Wordle
non_final_guess w@(Wordle c_g grn yel gry pool)
  -- At least (wordle_size) chars have not yet been guessed
  | length gry <= 26 - wordle_size =
      let n_g_pool = [c | c <- pool,
                          not (elem c gry),
                          not (elem c c_g),
                          not (elem c (keysAL yel))]
          n_g = take wordle_size n_g_pool
      in
        (Wordle n_g grn yel gry pool)
  -- There are still unguessed chars
  | length gry < 26 =
      let uniq_yel = nub (keysAL yel)
          uniq_grn = keysAL grn
          uniq_pool = [c | c <- pool,
                          not (elem c gry),
                          not (elem c uniq_grn),
                          not (elem c uniq_yel)]
          n_g = take wordle_size (uniq_pool ++ uniq_yel ++ uniq_grn)
      in
        (Wordle n_g grn yel gry pool)
  -- No unguessed chars, finalize the wordle
  | otherwise = final_guess w
          
-- Guess the word that makes the most sense with all the current info
final_guess :: Wordle -> Wordle
final_guess (Wordle c_g grn yel gry pool)
  | length yel > 0 = (Wordle (f_g_comb grn yel) grn yel gry pool)
  | otherwise = error "correct guess up to chance!"

-- combine final guess
f_g_comb :: [(Char, Int)] -> [(Char, Int)] -> String
f_g_comb g@((gc,gi):gt) y@((yc,yi):yt) =
  if gi < yi then
    gc : (f_g_comb gt y)
  else
    yc : (f_g_comb g yt)
f_g_comb _ _ = []

-- Execute the current guess for the game
-- DOES decrease turns
guess :: Game -> Game
guess g@(Game w@(Wordle c_g grn yel gry pool) k t)
  | t > 0 =
    let new_grn  = nub $ grn ++ (ud_grn c_g k 0) 
        new_gry  = nub $ gry ++ (ud_gry c_g k)
        new_yel  = nub $ yel ++ (ud_yel c_g k 0 new_grn new_gry)        
        new_pool = [p | p <- pool, not (elem p new_gry)]
    in
      Game (Wordle c_g new_grn new_yel new_gry new_pool) k (t-1)
  | otherwise = g


-- For a guess/key pair and starting index, produce the list of matching chars with index
ud_grn :: String -> String -> Int -> [(Char, Int)]
ud_grn (gh:gt) (kh:kt) i =
  if gh == kh then (gh,i) : (ud_grn gt kt (i+1))
  else (ud_grn gt kt (i+1))
ud_grn _ _ _ = []

-- For a guess/key pair and starting index, produce the list of chars that are in the
-- key but not in the correct position
ud_yel :: String -> String -> Int -> [(Char, Int)] -> [Char] -> [(Char, Int)]
ud_yel g k i grn gry =
  let known_chars = map fst grn
      known_i     = map snd grn
  in
    [(c,i) | c <- g, i <- [0 .. wordle_size],
             elem c k && not (elem i known_i),
             (countElem c known_chars < countElem c k)]

ud_gry :: String -> String -> [Char]
ud_gry g k = filter (\c -> not (elem c k)) g 


---------- TESTING ------------

-- A default Wordle to test with
def_w :: Wordle
def_w = Wordle "haskl" [] [] [] ['a' .. 'z']

-- A default Game to test with
def_g :: Game
def_g = Game def_w "cabal" 5

{-
What utility value function should we use for this example? For these wordles, I will define a reward function
that values the length of green, the length of yellow, and the length of the grey lists. Mathematically:
forall Wordle x: UV(x) = 100 * x.grn.length + 10 * x.yel.length + 1 * x.gry.length
-}

(grn_w, yel_w, gry_w) = (100,10,1)

w_uv :: Wordle -> Int
w_uv (Wordle c_g grn yel gry pool) =
  (grn_w * length grn) + (yel_w * length yel) + (gry_w * length gry)



{-
What neighborhood function should we use for this example? For these wordles, I will define a neighborhood function that produces all guesses that are one character off from the current guess of the input wordle. These neighboring Wordles have not actually guessed the new string yet; it is their *upcoming* guess.
-}

w_nb :: Wordle -> [Wordle]
w_nb w@(Wordle c_g grn yel gry pool) = [(Wordle s grn yel gry pool) | s <- string_nb c_g]

string_nb :: String -> [String]
string_nb s =
  -- replace all chars with list of all alternate chars
  let rep_w = map (\c1 -> (c1,[c2 | c2 <- ['a' .. 'z'], not (c1 == c2)])) s in
    [l | (c,l) <- rep_w] -- TODO: NEEDS FIXING

{-
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
-}
