---
author: 
 - Victor Cacciari Miraldo
title: The Ins and Outs of Generic Programming
subtitle: Hands-On Workshop @ Lambda World
institute: Utrecht University
theme: metropolis
mainfont: Open Sans
mainfontoptions: Scale=0.9
sansfont: Open Sans
sansfontoptions: Scale=0.9
monofont: Ubuntu Mono
monofontoptions: Scale=0.8
handout: true
natbib: true
biblio-title: Reading Material
bibliography: 
 - references.bib
---

## Preamble

\clonetherepo

## Motivation

Bob maintains networking code, Alice decides to add
another field to some datatype used indirectly.\pause

* No generics involved:
    + Compile-time failures if we have types\pause

* Generics involved:
    + Nothing happens, generic infrastructure handles this 
      automatically.

## Well Known Generic Problems

\pause

* Equality\pause

* Serialization\pause

* Pretty-Printing\pause

* Subterm Indexing\pause

* Merkle Trees\pause

* Differencing\pause

* etc

## Today

* Three Generic Programming Libraries
    - `GHC.Generics`, \pause the builtin generics powerhorse
    - `Generics.SOP`, \pause with expressive combinator-based programming
    - `Generics.MRSOP`, \pause combinator-based programming with mutual recursion



## Datatype Building Blocks

Datatypes can be constructed with sums, products the unit type
and the least fixpoint.

We can unwrap \emph{one layer} of a recursive type:

```haskell
data List a = Nil | Cons a (List a)

to :: Either () (a , List a) -> List a
to (Left ())       = Nil
to (Righ (x , xs)) = Cons x xs

from :: List a -> Either () (a , List a)
```

Or encode recursion explicitely:

```haskell
newtype Fix f = Fix (f (Fix f))
newtype ListF a x = ListF (Either () (a , x))
```

## Datatype Building Blocks

\exercise{LW2019/Prelude.hs}

\vfill

\pause

Meet the regular datatypes we will use today:
\emacsonly{LW2019/Types/Regular.hs}

## Datatype Building Blocks: Standardizing

`GHC.Generics` standard combinators instead of `Either`, `(,)`, ...

\vfill

```haskell
data (f :*: g) x = f x :*: g x
data (f :+: g) x = L1 (f x) | R1 (g x)
data U1        x = U1
data K1 i c    x = K1 x
data M1 i c f  x = M1 x
data V1        x 
```

\vfill

Let's write `GHC.Generic` instances.

\pause
\exercise{LW2019/Generics/GHC/Repr.hs}

## The Set of Regular Datatypes

* Lists, Binary Trees, etc... Constructed using sums, products, unit and least fixpoints.

* `GHC.Generics` \emph{does not} represent recursion explicitely.

* Standardized combinators allow us to write functions
  by \emph{induction on the structure of the generic representation}.

\pause

```haskell
instance (Func f , Func g) => Func (f :*: g) where
  func (fx :*: gx) = ...
```

\pause

\exercise{LW2019/Generics/GHC/Equality.hs}

## Sums-of-Products

* Writing functions by induction on the typeclass level is pretty boring.

* We know generic representation of a Haskell datatype will
  be in SOP form.

\pause

```haskell
data Bin a = Leaf a | Fork (Bin a) (Bin a)

type instance Code (Bin a) = '[ '[ a ]
                              , '[ Bin a , Bin a ] ]

type Rep (Bin a) = SOP I (Code (Bin a))
```

\exercise{LW2019/Generics/SOP/Repr.hs}

## Sums-of-Products: Interpreting Codes

Define GADT's that perform induction on \emph{codes}

```haskell
data NS (f :: k -> *) :: [k] where
  Z :: f x     -> NS f (x ': xs)
  S :: NS f xs -> NS f (x ': xs)

data NP (f :: K -> *) :: [k] where
  Nil  :: NP f []
  Cons :: f x -> NP f xs -> NP f (x ': xs)

newtype SOP f code = SOP (NS (NP f) codes)
```

Lets write the equality function for sums of products
\exercise{LW2019/Generics/SOP/Equality.hs} 

## Keep Note of These Types:

The `hcollapse` and `hczipWith` type signatures can hurt.

Here, we use them with the types:

```haskell
hcollapse :: NP (K a) xs -> [a]

hczipWith :: (forall x . Eq x => f x -> g x -> h x)
          -> NP f xs -> NP g xs -> NP h xs
```      

\pause

Is it possible to write a `hccollapse` version for `GHC.Generics`?

\vfill

## So Far

* Datatypes are built from standard combinators

* We can program by induction on this language

* Datatypes that come reified from Haskell are in
a normal form: SOP
    - Which enables for expressive combinators

\pause

\alert{What's Missing?}

* Recursion!

## Recursion Example

At what point do we start needing information about
recursive parts of a datatype?

```haskell
shapeEq [1,2,3] [5,6,7] == True
shapeEq [1,2,3] [5,6]   == False
```

\pause

What changes from regular equality? \pause Where we had,

```haskell
class GEq a where ...
```

\pause
We now track the recursive the type,

```haskell
class ShapeEq orig a where ...

class GShapeEq orig a where ...
  gshapeEq :: Proxy orig -> a -> a -> Bool
```

## Recursion Example: `GHC.Generics`

Example Instance Search:

```haskell
ShapeEq (Tree12 a)

GShapeEq (Tree12 a) (Rep (Tree12 a)) 

   GShapeEq (Tree12 a) (U1 :+: (K1 R a :*: K1 R (Tree12 a)) :+: ...)

      GShapeEq (Tree12 a) U1 -- Ok!

      GShapeEq (Tree12 a) (K1 R a :*: K1 R (Tree12 a))
 
         GShapeEq (Tree12 a) (K1 R a)          

         GShapeEq (Tree12 a) (K1 R (Tree12 a)) 
```

## Recursion Example: `GHC.Generics`

Use `OVERLAPPING` instances!

```haskell
instance {-# OVERLAPPING  #-} (ShapeEq orig orig) 
  => GShapeEq orig (K1 orig) where ...

instance {-# OVERLAPPABLE #-} GShapeEq orig (K1 a) where ...
``` 

\pause

\exercise{LW2019/Generics/GHC/ShapeEquality.hs}

\pause

And yes... We have to use the `orig` trick every time we need to
have information about which fields of a constructor are
recursive occurences of our type.

## Recursion Example: `SOP`

The `generics-sop` approach saves some work. We only need to do the
`orig` work once:

Define an annotated type.

```haskell
data Ann orig :: * -> * where
  Rec   :: orig -> Ann orig orig
  NoRec :: x    -> Ann orig x
```

And define instances to annotate $n$-ary products.

```haskell
class AnnotateRec orig (prod :: [ * ]) where
  annotate :: NP I prod -> NP (Ann orig) prod
```

\emacsonly{LW2019/Generics/SOP/AnnotateRec.hs}

## Recursion Example: `SOP`

Let's define shape equality for `SOP` and compare!

\exercise{LW2019/Generics/SOP/ShapeEquality.hs}

\pause

* How far is `Generics.GHC.Equality` to `Generics.GHC.ShapeEquality`?

\pause

* How far is `Generics.SOP.Equality` from `ShapeEquality`?
  
\pause

That's a consequence of the \emph{combinator based} approach, which
is only possible because generic types come in \emph{normal form} (SOP, in this case)

## Explicit Recursion

The `generics-mrsop` library supports Mutually Recursive Types,
which are a superset of the regular types.

\pause

Example:

* Regular: 
```haskell
data [a]     = [] | a : [a]
data Tree a  = Leaf | Bin a (Tree a) (Tree a)
data Maybe a = Nothing | Just a
```

* Mutually Recursive:
```haskell
data Zig = Zig | ZigZag Zag
data Zag = Zag | ZagZig Zig
```

## Codes for Mutually Recursive Types


```haskell
data Zig = Zig | ZigZag Zag
data Zag = Zag | ZagZig Zig
```

\pause

```haskell
type FamZig  = '[Zig , Zag]
```

\pause

```haskell
type CodeZig = '[ '[ '[] , '[ I 1 ] ]
                , '[ '[] , '[ I 0 ] ] ]
```

\pause

```haskell
type GZig = Rep NoOpq (Lkup FamZig) (Lkup CodesZig 0)
```

\pause
```haskell
newtype Rep ki f code = Rep (NS (NP (NA ki f)) code
```

```haskell
data NA ki f :: Atom -> * where
  NA_I :: f ix -> NA (I ix)
  NA_K :: ki k -> NA (K k)
```



## The Sugar-free `Generic` Class

```haskell
class Family (ki :: kon -> *) (fam :: [*]) (codes :: [[[Atom kon]]])
  where
    sfrom' :: SNat ix -> El fam ix -> Rep ki (El fam) (Lkup ix codes)

    sto'   :: SNat ix -> Rep ki (El fam) (Lkup ix codes) -> El fam ix
```

\pause

```haskell
data SNat :: Nat -> * where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

data El :: [k] -> Nat -> k where
  El :: Lkup fam ix -> El fam ix

```

\pause

\exercise{LW2019/Generic/MRSOP/Repr.hs}

## Equalities in `generics-mrsop`

\exercise{LW2019/Generic/MRSOP/Equality.hs}
\exercise{LW2019/Generic/MRSOP/ShapeEquality.hs}

```haskell
zipRep :: Rep ki f c -> Rep kj g c 
       -> Maybe (Rep (ki :*: kj) (f :*: g) c) 

elimRep :: (forall k.  ki k  -> a)  -- eliminate opaques
        -> (forall ix. f  ix -> a)  -- eliminate recursive positions
        -> ([a] -> b)               -- combine the eliminated fields 
        -> Rep ki f c               -- value we want to eliminate
        -> b       
```

## Catamorphisms (AKA `fold`)

Explicit Recursion enables generic recursion schemes!

```haskell
cata :: (forall iy. Rep ki phi (Lkup iy codes) -> phi iy) 
     -> Fix ki codes ix
     -> phi ix
```

\exercise{LW2019/Generic/MRSOP/Height.hs}


## Annotated Fixpoints

Instead of consuming a type, we can choose to keep the intermediary
results annotated in the tree.


```haskell
newtype AnnFix phi f = AnnFix (phi , f (AnnFix phi f))
```

## Annotated Fixpoints: `generics-mrsop`

In `mrsop`, we need a slightly more complicated type, because
of the indicies involved.

\pause

```haskell
synthesize :: (forall iy . Rep ki phi (Lkup iy codes) -> phi iy)
           -> Fix ki codes ix
           -> AnnFix ki codes phi ix
```

where

```haskell
newtype AnnFix ki codes (phi :: Nat -> *) ix = ...
```

\pause

\emacsonly{LW2019/Generics/MRSOP/MerkleTree.hs}

## Advanced Material

If you are into this kind of things, make sure to check
the rest of the repository. For example,

\emacsonly{LW2019/Generics/MRSOP/Arbitrary.hs}

## Summary

\begin{figure}\centering
\begin{tabular}{@{}lll@{}}\toprule
                        & Pattern Functors       & Codes                 \\ \midrule
  No Explicit Recursion & \texttt{GHC.Generics}\cite{Magalhaes2010}  & \texttt{generics-sop}\cite{deVries2014} \\
  Simple Recursion      &  \texttt{regular}\cite{vanNoort2010}      &  \multirow{2}{*}{\texttt{generics-mrsop}\cite{Miraldo2018}} \\
  Mutual Recursion      &  \texttt{multirec}\cite{Yakushev2009}     &   \\
\bottomrule
\end{tabular}
\end{figure}

\pause

Other approaches include support for GADT's\cite{Serrano2018} and higher kinded classes\cite{Serrano2019}.