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
sansfontoptions: Scale=0.8
monofont: Ubuntu Mono
monofontoptions: Scale=0.8
---

# Generic Programming

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

* Important: Clone the repository
    - `https://github.com/VictorCMiraldo/lw2019-generics-workshop.git`

# Building Datatypes 101

## Datatype Building Blocks

Datatypes can be constructed with sums, products and the unit type.

Lets to the Prelude!

```haskell
data (f :*: g) x = f x :*: g x
data (f :+: g) x = L1 (f x) | R1 (g x)
data U1        x = U1
data K1 m a    x = K1 x
data M1 i s c  x = M1 x
```

REVIEW THESE DEFS!!!


## The Set of Regular Datatypes

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
