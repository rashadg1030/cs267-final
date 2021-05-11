# Parallelizing Type Checking

## Introduction

For my CS267 final project on parallel computation, I'll be discovering the benefits that come with parallelizing algorithms used for type checking programs. In modern software engineering, we've seen a big resurgance of the use of *static types* or *type annotations* to statically analyze programs. Not only have we seen an increase in the revelance of statically typed languages like [Haskell](), [OCaml](), and [ReScript](), but we also see many examples of tooling specifically being built to bring the benefits of type checking to dynamically typed languages. [TypeScript](), [Flow](), and [MyPy]() are just a few well-known examples. The goal of this paper is to show: the relevance of parallel computation to the static analysis of programs, how parallelism looks in a high-level, functional language (Haskell), and how parallelizing type checking algorithms is not only intuitive but offers worthy perfomance benefits as well.

## Hypothesis

My hypothesis is that parallelzing type checking brings great performance benefits while not complicating the implementation of the typechecking algorithm much, if at all. I also plan on testing the algorithm on at least two type sytems to compare and contrast the effeciency gains.

## Context

I will be working on this project alone. The project is not a subproject of another, and any derivatives of this project will not be used in other classes.

## Key Prior Work

This project can be contrasted with the paper [Parallel type-checking with haskell using saturating LVars and stream generators](https://dl.acm.org/doi/10.1145/2851141.2851142), which this paper is inspired by. The primary difference is that this project's implementation will not use saturated LVars or stream generators to handle parallelism, but instead use the Par monad, which is much more developer friendly.

## Empirical Methodology

My methodology for measuring the effectiveness of parallelized type checking involves the following steps:

1. Define a type system and syntax that the serial and parallel implementations will be based on. This includes the grammar, syntax, parser, etc. For this project, I will use the [simply typed calculus]() extended with some primitive types to start off. I can extend the complexity of the type system for more interesting results.

```
-- This is a comment

{- This is a mutliline comment

Primtive types are:

Bool
Int
Float
Char
->
List A

Primitive values and functions that can only be used in refinements are:

Integers: ..., -3, -2, -1, 0, 1, 2, 3, ...
Booleans: True, False
Operators on Integers: >, <, >=, <=, ==, !=, +, -, *
Operators on Booleans: and, or, not
Operators on List: len (for getting the length)
-}

String := List Char

-- Sum Type

Answer := Yes :+ No :+ Idk

-- Sum Type with Type Parameter

Maybe A := Just A :+ Nothing

-- Product Type

FullName := String :* String

-- Record (Syntactic sugar for product type)

FullName' := { first : String, last : String }

-- Refinement Types

Nat := { n : Int | n > 0 }

FullName'' :=
  { first : String
  , last : String
  | len first > 6
  }

-- Be able to create bindings
someName : String

someAge : Nat

someAge' : { v : Int | v >= 0 }

someAmount : { m : Float | True }

guessAge : Float -> String -> Nat

-- Then below after you specify the types, you can write a program that the type checker will check

guessAge someAmount someName -- Type checks

guessAge someName someAmount -- Doesn't type check

```

2. Write a serial version of the type checker, and measure the time it takes to type check a specified test file. The test file should be of reasonable length and have simple to complex type annotations that the type checker will have to check.

3. Write a parallel version of the type checker, and measure the time it takes to do the same task in (2) on the same file. Test the parallel version on 1, 2, 3, 4, 5, 6, 7, 8 processors and record the times of each one.

4. Gather and compare results. Try to reason about why and how performance was gained or lost in the parallel version. How does the program scale to multiple cores? How does the size of the input affect times? There are many questions that need to be considered.

Resources I will need are a computer or machine that offers up to 8 processors or I can just test up to 4 processors on my laptop.

## Challenges

Challenges for this project will be first implementing a simple lexer and parser for the type checkers to be tested on. This is the first step. After this I must implement a type checker for the simply typed lambda calculus, plus some possible extensions. This will be the serial version used as a base line. Next, is using the [monad-par]() library to implement the parallel version of the type checker. I will have to learn how to use `monad-par`, but it seems like it has an easy to use interface from what I've read so far. Finally, figuring out how to profile and measure the performance of a parallel Haskell program. There is a lot of documentation on the topic, so it should be straightforward. All in all, although there are many challenges, I have the resources to overcome them and make this an interesting project.

## What are types?

## What is type checking?

## Parallelism in Haskell: A Primer

## Opportunities for parallelization.

