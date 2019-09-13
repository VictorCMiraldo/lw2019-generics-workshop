---
author: 
 - Victor Cacciari Miraldo
title: The Ins and Outs of Generic Programming
subtitle: Hands-On Workshop @ Lambda World
institute: Utrecht University
theme: metropolis
mainfont: Open Sans
mainfontoptions: Scale=0.8
sansfont: Open Sans
sansfontoptions: Scale=0.6
monofont: Ubuntu Mono
monofontoptions: Scale=0.7
handout: true
---

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

\clonetherepo

* Three Generic Programming Libraries
    - `GHC.Generics`, \pause the builtin generics powerhorse
    - `Generics.SOP`, \pause with expressive combinator-based programming
    - `Generics.MRSOP`, \pause combinator-based programming with mutual recursion

* Important: Clone the repository
    - `https://github.com/VictorCMiraldo/lw2019-generics-workshop.git`

## Datatype Building Blocks

Datatypes can be constructed with sums, products and the unit type.

\vfill

\exercise{LW2019/Prelude.hs}

\vfill

Note the `from` and `to` functions converting between generic
and original representations.

\pause

Meet the regular datatypes we will use today:
\emacsonly{LW2019/Types/Regular.hs}

## Datatype Building Blocks: Standardizing

`GHC.Generics` standard combinators:

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

* Lists, Binary Trees, etc... Constructed using sums, products, unit and least fixpoints:
    ```haskell
    newtype Fix f = Fix { unFix :: f (Fix f) }
    ```

* `GHC.Generics` does not represent recursion explicitely.

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
```

Lets write the equality function for sums of products
\exercise{LW2019/Generics/SOP/Equality.hs}        

## The Set of Mutually Recursive Datatypes

## General Regular Datatypes

* Akin to regular languages
* Simple recursion at most
* Break simplistic pattern with
  shape-equality. The Lack of knowledge about what
  is recursive data or not will make this very hard to write in
  GHC.Generics or Generics.SOP

## Generic Equality

## Generic Serialization

## Sums of Products of Regular Datatypes

* Equality revisite
* Serialization revisited


## Datatypes Building Blocks

## Normal Forms for Building Blocks

## Recursion

## Mutual Recursion
