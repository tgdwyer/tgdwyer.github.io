---
layout: page
title: "Parser Combinators"
permalink: /parsercombinators/
---
***under construction...***

## Learning Outcomes

- Understand that a parser is a program which extracts information from structured text
- Apply what we have learned about Haskell typeclasses and other functional programming concepts to create solutions to real-world problems
- In particular, we learn to use parser combinators and see how they are put together

## Introduction

In this section we will see how the various Haskell language features we have explored allow us to solve real-world problems.  In particular, we will develop a simple but powerful library for building *parsers* that is compositional through [Functor](/haskell3/#functor), [Applicative](/haskell3/#applicative) and [Monad](/haskell4/#monad) interfaces.  Before this, though, we will learn the basics of parsing text, including a high-level understanding that parsers are *state-machines* which realise a *context-free grammar* over a textual language.

In this chapter, a *parser* is simply a function which takes a string as input and produces some structure or computation as output.
Parsing has a long history and *parser combinators* are a relatively recent approach made popular quite recently by modern functional programming techniques.  
A *parser combinator* is a [higher-order function](/higherorderfunctions) that accepts parsers as input and combines them somehow into a new parser.

More traditional approaches to parsing typically involve special purpose programs called *parser generators*, which take as input a grammar defined in a special language (usually some derivation of BNF as described below) and generate the partial program in the desired programming language which must then be completed by the programmer to parse such input.  Parser combinators have the advantage that they are entirely written in the one language.  Parser combinators written in Haskell take advantage of the expressiveness of the Haskell language such that the finished parser can look a lot like a BNF grammar definition, as we shall see.

The parser combinator discussed here is based on one developed by Tony Morris and Mark Hibberd as part of their ["System F" Functional Programming Course](https://github.com/system-f/fp-course), which in turn is a simplified version of official Haskell parser combinators such as [parsec](https://hackage.haskell.org/package/parsec) by Daan Leijen.

You can play with the example and the various parser bits and pieces in [this on-line playground](https://repl.it/@tgdwyer/DistortedMaroonDigits#main.hs).

## Context-free Grammars and BNF

Fundamental to analysis of human natural language but also to the design of programming languages is the idea of a *grammar*, or a set of rules for how elements of the language may be composed.  A context-free grammar (CFG) is one in which the set of rules for what is produced for a given input (*production rules*) completely cover the set of possible input symbols (i.e. there is no additional context required to parse the input).  Backus-Naur Form (or BNF) is a notation that has become standard for writing CFGs since the 1960s.  We will use BNF notation from now on.  There are two types of symbols in a CFG: *terminal* and *non-terminal*.  In BNF non-terminal symbols are `<nameInsideAngleBrackets>` and can be converted into a mixture of terminals and/or nonterminals by production rules:

```
<nonterminal> ::= a mixture of terminals and <nonterminal>s, alternatives separated by |
```

Thus, *terminals* may only appear on the right-hand side of a production rule, *non-terminals* on either side.  In BNF each *non-terminal* symbol appears on the left-hand side of exactly one production rule, and there may be several possible alternatives for each *non-terminal* specified on the right-hand side.  These are separated by a "`|`" (in this regard they look a bit like the syntax for [algebraic data type definitions](https://tgdwyer.github.io/haskell2#algebraic-data-types)).

Note that production rules of the form above are for context-free grammars.  As a definition by counter-example, *context sensitive grammars* allow terminals and more than one non-terminal on the left hand side.

Here's an example BNF grammar for parsing Australian land-line phone numbers, which may optionally include a two-digit area code in brackets, and then two groups of four digits, with an arbitrary number of spaces separating each of these, e.g.:

> (03) 9583 1762  
> 9583 1762

Here's the BNF grammar:

```
<phoneNumber> ::= <fullNumber> | <basicNumber>
<fullNumber> ::= <areaCode> <basicNumber>
<basicNumber> ::= <spaces> <fourDigits> <spaces> <fourDigits>
<fourDigits> ::= <digit> <digit> <digit> <digit>
<areaCode> ::= "(" <digit> <digit> ")"
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<spaces> ::= " " <spaces> | ""
```

So `"0"`-`"9"` and `"("`, `")"` are the full set of terminals.

Now here's a sneak peak at a simple parser for such phone numbers.  It succeeds for any input string which satisfies the above grammar, returning a 10-digit string for the full number without spaces and assumes "03" for the area code for numbers with none specified (i.e. it assumes they are local to Victoria).  We can use it like so:

```haskell
GHCi> parse phoneNumber "(02)9583 1762"
Result >< "0295831762"

GHCi> parse phoneNumber "9583  1762"
Result >< "0395831762"

GHCi> parse phoneNumber "9583-1762"
Unexpected character: "-"
```

We haven't bothered to show the types for each of the functions in the code below, as they are all `::Parser [Char]` - meaning a Parser that returns a string.  We'll explain all the types and functions used in due course.  For now, just notice how similar the code is to the BNF grammar definition:

```haskell
phoneNumber = fullNumber ||| (("03"++) <$> basicNumber)

fullNumber = do
   ac <- areaCode
   n <- basicNumber
   pure (ac ++ n)

basicNumber = do
   spaces
   first <- fourDigits
   spaces
   second <- fourDigits
   pure (first ++ second)

fourDigits = do
  a <- digit
  b <- digit
  c <- digit
  d <- digit
  pure [a,b,c,d]

areaCode = do
  is '('
  a <- digit
  b <- digit
  is ')'
  pure [a,b]
```

## Parser Type

At essence, our parser is going to be summed up by a couple of types:

```haskell
type Input = String
newtype Parser a = P { parse :: Input -> ParseResult a}
```

We assume all `Input` is a `String`, i.e. Haskell's basic builtin `String` which is a list of `Char`.

Then the `Parser` type has one field `parse` which is a function of type `Input -> ParseResult a`.  So it parses strings and produces parse results, where a Parse result is:

```haskell
 data ParseResult a = Error ParseError
                    | Result Input a
  deriving Eq
```

We'll come back to the `ParseError` type - which will be returned in the case of unexpected input, but we can see that a successful Parse is going to produce a `Result` which has two fields - more `Input` (the part of the input remaining after we took a bit off and parsed it), and an `a` - a type parameter that we may specify for concrete `Parser` instances.

So this is pretty abstract.  It says nothing about what precise `Input` string we are going to parse, or what type `a` we are going to return as a result.

## Instances

## Error Handling

```haskell
data ParseError = UnexpectedEof
                | UnexpectedChar Char
  deriving (Eq, Show)
```

## Parser Combinators

The most atomic function for a parser of `String`, is to pull a single character off the input.  The only thing that could go wrong is to find our input is empty.

```haskell
character :: Parser Char
character = P parseit
  where parseit "" = Error UnexpectedEof
        parseit (c:s) = Result s c
```

The following is how we will report an error when we encounter a character we didn't expect.  This is not the logic for recognising a character, that's already happened and failed and the unrecognised character is now the parameter.  This is just error reporting, and since we have to do it from within the context of a `Parser`, we create one using the `P` constructor.  Then we set up the one field common to any `Parser`, a function which returns a `ParseResult` -- no matter the input, hence `const`.  The rest creates the right type of `Error` for the given `Char`.

```haskell
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = P . const . Error . UnexpectedChar
```

Now a parser that insists on a certain character being the next one on the input.  It's using the `Parser` instance of `Monad`'s bind function (implicitly in [a `do` block](/monad/#do-notation)) to sequence first the `character` `Parser`, then either return the correct character in the `Parser`, or the `Error` parser.

```haskell
is :: Char -> Parser Char
is c = do
  v <- character
  let next = if v == c
             then pure
             else const $ unexpectedCharParser v
  next c
```

And finally our `Parser` for trying to apply a first `Parser`, and then and alternate `Parser` if the first fails:

```haskell
(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 = P (\i -> let f (Error _) = parse p2 i
                         f r = r
                     in f $ parse p1 i)
```

## Nitty gritty

The last two pieces of our Phone Numbers grammar we also implement fairly straightforwardly from the BNF.
```
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<spaces> ::= " " <spaces> | ""
```

Here's a trivial adaptation of `digit`:
```haskell
digit :: Parser Char
digit = is '0' ||| is '1' ||| is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6' ||| is '7' ||| is '8' ||| is '9'
```

Spaces is a bit more interesting because it's recursive, but still almost identical to the BNF:

```haskell
spaces :: Parser ()
spaces = (is ' ' >> spaces) ||| pure ()
```

### Exercises

- make a less repeatitive `digit` parser by creating a function `satisfy :: (Char -> Bool) -> Parser Char` which returns a parser that produces a character but fails if: the input is empty; or the character does not satisfy the given predicate. You can use the `isDigit` function from `Data.Char` as the predicate.

- change the type of `spaces` to `Parser [Char]` and have it return the appropriately sized string of only spaces.

## Creating a Parse Tree

A BNF grammar for a simple calculator with three operations `*`, `+` and `-`, with `*` having higher precedence than `+` or `-`:
```
<expr> ::= <term> { <add> <term> }
<term> ::= <number> { "*" <number> }
<add> ::= "+" | "-"
```


```haskell
data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Number Integer
deriving Show
```

```haskell
add :: Parser (Expr -> Expr -> Expr)
add = (op '+' >> pure Plus) ||| (op '-' >> pure Minus)

times :: Parser (Expr -> Expr -> Expr) 
times = op '*' >> pure Times

expr :: Parser Expr 
expr = chain term add 

term :: Parser Expr 
term = chain number times

-- | Parse an expression of integers, +, -, *
-- >>> parseCalc " 12 + 21* 3 "
-- Result > < Plus (Number 12) (Times (Number 21) (Number 3))
--
-- >>> parseCalc " 6 *4 + 333- 8 *  24"
-- Result >< Minus (Plus (Times (Number 6) (Number 4)) (Number 333)) (Times (Number 8) (Number 24))
--
parseCalc :: String -> ParseResult Expr
parseCalc = parse expr 

op :: Char -> Parser Char -- parse a single char operator
op c = do
   spaces
   charTok c
   pure c
  
number :: Parser Expr     -- parse a Number
number = spaces >> Number <$> read <$> list1 digit

chain :: Parser a -> Parser (a->a->a) -> Parser a -- chain p op parses 1 or more instances of p 
chain p op = p >>= rest                           -- separated by op
   where                                          -- (see chainl1 from Text.Parsec)
   rest a = (do
               f <- op
               b <- p
               rest (f a b)
            ) ||| pure a
```
