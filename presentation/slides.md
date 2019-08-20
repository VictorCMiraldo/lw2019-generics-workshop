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

* Clone the repository

* Profit

# Building Datatypes 101

## The Set of Non-recursive Datatypes

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
