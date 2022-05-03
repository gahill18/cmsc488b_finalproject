
# Table of Contents

1.  [Introduction](#org92a3fe0)
2.  [Usage](#org3c10cb0)
3.  [Examples](#org9fc2ab9)



<a id="org92a3fe0"></a>

# Introduction

Targeted property based testing, or t-PBT, is a library designed to make property based testing with quickCheck produce counterexamples to defined properties more efficiently. It does so in the following steps:

1.  use quickCheck's arbitrary to define a starting point (users can define their own arbitrary instaces for non-prelude types)
2.  use a user-provided neighborhood function to get a list of nearby points
3.  use a user-provided utility value (uv) function to search for a local min or max
4.  if the value of the local min or max violates the property you are looking for, congratulations you have a counterexample!
    otherwise, take the local min or max and repeat steps 2 through 4 until either
    
    a. the difference between iterations is too small to be meaningful
    
    b. you run out of "gas", which the user defines as the number of iterations before quitting


<a id="org3c10cb0"></a>

# Usage

The TargetedPBT module has four primary functions for t-PBT:

1.  maxUVover    :: (Show a, Arbitrary a , Ord b) => (a -> b) -> (a -> [a]) -> b -> Int -> IO ()
2.  maxUVUnder   :: (Show a, Arbitrary a , Ord b) => (a -> b) -> (a -> [a]) -> b -> Int -> IO ()
3.  minUVOver    :: (Show a, Arbitrary a , Ord b) => (a -> b) -> (a -> [a]) -> b -> Int -> IO ()
4.  minUVUnder   :: (Show a, Arbitrary a , Ord b) => (a -> b) -> (a -> [a]) -> b -> Int -> IO ()

These functions take the appropriate user defined functions and perform the steps listed above for each arbitrary starting point produced by quickCheck

a          must implement Show and Arbitrary to satisfy quickCheck types

b          must be orderable to find the max or min value

(a -> b)   is the uv function

(a -> [a]) is the neighborhood function

b          is the threshold you are comparing the output of the uv function to

Int        is the gas, or number of iterations you want to search before quitting

IO ()      is the same as the output of quickCheck, because it IS the output of a quickCheck


<a id="org9fc2ab9"></a>

# Examples

Aside from the very basic example given in TargetedPBT.hs, a demonsration of using t-PBT on arbitrary Wordle gamestates is given in Wordle.hs

