# Exercise Plan

## Intro

* Expose the overall idea: We can do induction on a datatype's
  shape. `LW2019.Prelude`

* Talk about classes of datatypes, present first class:
  `LW2019.Types.Regular`

## GHC.Generics

* Show the lenses through which `GHC.Generics` sees the
  (data) world. `LW2019.Generics.GHC.Repr`

* Do equality. Argue that defining functions by induction on
  arbitrary datatype shapes is nonsense. Haskell datatypes
  come in Sums-of-Products anyway, right?

* Talk about lack of recursive structure.

## Generics.SOP

* Show the SOP representation of datatypes. 
  - Present NS, NP and I; define SOP.
  
* TODO: should I show a simple example before doing equality ?

* Do equality

## GHC.Generics 

* Do shape equality

## Generics.SOP

* Do Shape equality? (how?)

## MRSOP

* This brings us to MRSOP! Having access to the recursive structure
  of a datatype is pretty important.


... 
  

