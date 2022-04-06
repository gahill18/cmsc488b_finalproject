{-

Garrett Hill
CMSC488B
Spring 2022

------------------ DISCLAIMERS --------------------
This library uses suboptimal algorithms that would likely slow down significantly for larger data sets.
It serves more as a proof of concept that is readable and PROVABLY correct within user defined margins.
Caveat emptor.
-}

module TargetedPBT where

import Data.List
import Test.QuickCheck

{-
Takes in a function to calculate uvs, a function to calculate a set of nearby points from a given point, and an initial guess point.
Returns (local maximum, corresponding uv)
-}
maximize :: Ord b => (a -> b) -> (a -> [a]) -> a -> (a, b)
maximize f_uv f_wiggle init =
  let is = f_wiggle init
      uvs = map f_uv is
      l = zip is uvs
  in tupleMax l

{-
Takes in a list of tuples, of which the second type needs to be orderable in order to sort.
Returns the tuple with the largest second value.
-}
tupleMax :: Ord b => [(a, b)] -> (a, b)
tupleMax [] = error "tupleMax passed empty list!"
tupleMax [x] = x
tupleMax ((a,b):t) = let (a',b') = tupleMax t in
  if b < b' then (a',b')
  else (a,b)


{-
Takes in a function to calculate uvs, a function to calculate a set of nearby points from a given point, and an initial guess point.
Returns (local minimum, corresponding uv)
-}
minimize :: Ord b => (a -> b) -> (a -> [a]) -> a -> (a, b)
minimize f_uv f_wiggle init =
  let is = f_wiggle init
      uvs = map f_uv is
      l = zip is uvs
  in tupleMin l

{-
Takes in a list of tuples, of which the second type needs to be orderable in order to sort.
Returns the tuple with the lowest second value.
-}
tupleMin :: Ord b => [(a, b)] -> (a, b)
tupleMin [] = error "tupleMin passed empty list!"
tupleMin [x] = x
tupleMin ((a,b):t) = let (a',b') = tupleMin t in
  if b > b' then (a',b')
  else (a,b)

{-
Takes a uv function, a neighborhood (wiggle) function, an initial guess,
and a gas level. Gas level is number of steps before termination if no
local minimum has been found by that point.
Returns (a local minimum, associated utility value).
-}
gradientDescent :: Ord b => (a -> b) -> (a -> [a]) -> a -> Int -> (a, b)
gradientDescent f_uv f_wiggle i gas | gas > 0 =
  let (new_i,new_uv) = minimize f_uv f_wiggle i in
    if new_uv == f_uv i then (new_i,new_uv)
    else gradientDescent f_uv f_wiggle new_i (gas - 1)
gradientDescent f_uv f_wiggle i _ = (i, f_uv i)

{-
Takes a uv function, a neighborhood (wiggle) function, an initial guess,
and a gas level. Gas level is number of steps before termination if no
local maximum has been found by that point.
Returns (a local maximum, associated utility value).
-}
gradientAscent :: Ord b => (a -> b) -> (a -> [a]) -> a -> Int -> (a, b)
gradientAscent f_uv f_wiggle i gas | gas > 0=
  let (new_i,new_uv) = maximize f_uv f_wiggle i in
    if new_uv == f_uv i then (new_i,new_uv)
    else gradientAscent f_uv f_wiggle new_i (gas - 1)
gradientAscent f_uv f_wiggle i _ = (i, f_uv i)


{-
UV is a utility value function
It produces a "ranking" for any given input,
i.e. a value that is above or below its nearby peers
(it is ASSUMED that the UV function is continuous, otherwise
this calculation would not make sense to perform)
-}
sampleUV :: Int -> Int -- a = Int, b = Int from gradientDescent
sampleUV i = (10 - i) ^ 2 -- (how far i is from 10) squared

prop_sampleUVPosNegEqual :: Int -> Property
-- any value that is x distance away from 10 should produce the same uv, whether greater or less than 10
prop_sampleUVPosNegEqual i = counterexample "pos/neg distance not equal" $ sampleUV (10-i) == sampleUV (10+i)


{-
Wiggle is a neighborhood function
It produces a series of 'nearby 'data points
-}
sampleWiggle :: Int -> Int -> [Int] -- a = Int, [a] = [Int] from gradientDescent
sampleWiggle wiggleDistance i = [(i - wiggleDistance) .. (i + wiggleDistance)]
-- The list of ints between i +/- wiggleDistance


prop_sampleWiggleDist :: Int -> Int -> Property
-- all values in the sample wiggle distance should be less than the randomly chosen wiggle distance away from i
prop_sampleWiggleDist d i = counterexample ((show i) ++ " wiggled farther than " ++ (show d)) $
  all (\x -> abs (x - i) <= d) (sampleWiggle d i)


-- give an initial integer guess i, and a step count g, run the gradient descent for i,g
-- this example uses the numbers within +/- 10 of i for its neighborhood function
sampleGD :: Int -> Int -> (Int, Int)
sampleGD i g = gradientDescent sampleUV (sampleWiggle 10) i g


prop_sampleGDCloseEnoughAnswer :: Int -> Int -> Property
-- If gas > 0, the new_i should be within gas*10 of the value 10
prop_sampleGDCloseEnoughAnswer i g | g > 0 = let (new_i, new_uv) = sampleGD i g in
  counterexample ("new_i: " ++ (show new_i) ++ ", new_uv: " ++ (show new_uv) ++ ", g: " ++ (show g)) $
  (10 - new_i) `div` 10 <= g
-- if gas <= 0, then new_i should equal the original i
prop_sampleGDCloseEnoughAnswer i g = let (new_i, new_uv) = sampleGD i g in
  counterexample ("new_i: " ++ (show new_i) ++ ", new_uv: " ++ (show new_uv) ++ ", g: " ++ (show g)) $
  (new_i == i)
  

{-
experiment with expanding gradient descent to accept functions
with unlimited arguments

roadmap:
implement typeclass to demonstrate properties of some object's state
- need to include utility function and neighborhood function
- implement arbitrary so that you can generate instances of the objects
- for every test that quickcheck generates an arbitrary instance for, gradientDescent to check nearby points for higher likelhihood to fail
-}
