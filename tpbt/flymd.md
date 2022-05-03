#+title: tpbt
#+author: Garrett Hill

* Introduction

   Targeted property based testing, or t-PBT, is a library designed to make property based testing with quickCheck produce counterexamples to defined properties more efficiently. It does so in the following steps:
   
   1. use quickCheck's arbitrary to define a starting point
      (users can define their own arbitrary instaces for non-prelude types)
   2. use a user-provided neighborhood function to get a list of nearby points
   3. use a user-provided utility function 

* Usage
fLyMd-mAkEr
* Examples
