---
layout: page
title: "Parser Combinators"
permalink: /parsercombinators/
---
under construction...

## Learning Outcomes

- Understand that a parser is a program which extracts information from a structured text file
- Apply what we have learned about Haskell typeclasses and other functional programming concepts to create solutions to real-world problems
- In particular, we learn to use Parser combinators and see how they are put together

## Introduction

## Context-free Grammars

## Backus Naur Form (BNF)

## Parser Type

At essence, our parser is going to be summed up by a couple of types:

```haskell
type Input = String
newtype Parser a = P { parse :: Input -> ParseResult a}
```

We assume all `Input` is a `String`, i.e. Haskell's basic builtin `String` which is a list of `Char`.

Then the `Parser` type has one field `parse` which is a function of type `Input -> ParseResult a`.  So it parses strings and produces parse results, where a Parse result is:

```haskell
 data ParseResult a =
    Error ParseError
  | Result Input a
  deriving Eq
```

We'll come back to the `ParseError` type - which will be returned in the case of unexpected input, but we can see that a successful Parse is going to produce a `Result` which has two fields - more `Input` (the part of the input remaining after we took a bit off and parsed it), and an `a` - a type parameter that we may specify for concrete `Parser` instances.

So this is pretty abstract.  It says nothing about how big of a piece of the `Input` string we are going to parse, or exactly what type `a` we are going to make out of it.
