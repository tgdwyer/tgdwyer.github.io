---
layout: page
title: "Parser Combinators"
permalink: /parsercombinators/
---

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

Now here's a sneak peak at a simple parser for such phone numbers.  It succeeds for any input string which satisfies the above grammar, returning a 10-digit string for the full number without spaces and assumes "03" for the area code for numbers with none specified (i.e. it assumes they are local to Victoria).  Our [`Parser` type](/parsercombinators/#parser-type) provides a function `parse` which we call like so:

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

The `Parser` and the `ParseResult` types are pretty abstract.  They say nothing about what precise `Input` string we are going to parse, or what type `a` we are going to return in the result.  This is the strength of the parser, allowing us to build up sophisticated parsers for different input grammars through composition using [instances of Function, Applicative and Monad](/parsercombinators/#instances), and the `ParseResult` parameter `a` allows us to produce whatever we want from the parsers we create.

## Error Handling

Error handling is a very important part of any real-world parser.  Decent error reporting allows us to quickly diagnose problems in our input.
As we saw above a `ParseResult` may be either a successful `Result` or an `Error`, the latter containing information in a `ParseError` data structure about the nature of the error.

```haskell
data ParseError =
    UnexpectedEof -- hit end of file when we expected more input
  | ExpectedEof Input -- should have successfully parsed everything but there's more!
  | UnexpectedChar Char
  | UnexpectedString String
  deriving (Eq, Show)
```

Naturally it needs to be `Show`able, but also `Eq`uality testable so that we can pattern match `ParseResult` to handle particular types of errors.

## Instances

First an instance of `Show` to pretty print the `ParseResult`s:

```haskell
instance Show a => Show (ParseResult a) where
  show (Result i a)                 = "Result >" ++ i ++ "< " ++ show a
  show (Error UnexpectedEof)        = "Unexpected end of stream"
  show (Error (UnexpectedChar c))   = "Unexpected character: " ++ show [c]
  show (Error (UnexpectedString s)) = "Unexpected string: " ++ show s
  show (Error (ExpectedEof i))      =
    "Expected end of stream, but got >" ++ show i ++ "<"
```

And `ParseResult` is also an instance of `Functor` so that we can map functions over the output of a successful parse - or nothing if the result is an `Error`:

```haskell
instance Functor ParseResult where
  fmap f (Result i a) = Result i (f a)
  fmap _ (Error e)    = Error e
```

A `Parser` itself is also a `Functor`.  This allows us to create a new `Parser` by composing functionality onto the `parse` function for a given `Parser`:

```haskell
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P p) = P (fmap f . p)
```

The applicative `pure` creates a Parser that always succeeds with the given input, and thus forms a basis for composition.  We saw it being used in the above example to return the results of a parse back into the `Parser` at the end of a `do`-block.  

The `(<*>)` allows us to map functions in the `Parser` over another `Parser`.  As with other `Applicative` instances, a common use case would be composition with a `Parser` that returns a data constructor as we will see in [the next example](/parsercombinators/#creating-a-parse-tree).

```haskell
instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P (`Result` x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p q = p >>= (\f -> q >>= (pure . f))
```

The `Monad` instance's bind function `(>>=)` we have already seen in use in the example above, allowing us to sequence `Parser`s in `do`-blocks to build up the implementation of the `BNF` grammar.

```haskell
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (P p) f = P (
    \i -> case p i of
      Result rest x -> parse (f x) rest
      Error e -> Error e)
```

## Parser Combinators

The most atomic function for a parser of `String`, is to pull a single character off the input.  The only thing that could go wrong is to find our input is empty.

```haskell
character :: Parser Char
character = P parseit
  where parseit "" = Error UnexpectedEof
        parseit (c:s) = Result s c
```

The following is how we will report an error when we encounter a character we didn't expect.  This is not the logic for recognising a character, that's already happened and failed and the unrecognised character is now the parameter.  This is just error reporting, and since we have to do it from within the context of a `Parser`, we create one using the `P` constructor.  Then we set up the one field common to any `Parser`, a function which returns a `ParseResult` no matter the input, hence `const`.  The rest creates the right type of `Error` for the given `Char`.

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

And finally our `Parser` for trying to apply a first `Parser`, and then and alternate `Parser` if the first fails.  This allows us to encode the alternatives in our BNF grammar rules.

```haskell
(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 = P (\i -> let f (Error _) = parse p2 i
                         f r = r
                     in f $ parse p1 i)
```

## Nitty gritty

The last two pieces of our Phone Numbers grammar we also implement fairly straightforwardly from the BNF.  In a real parser combinator library you'd do it differently, as per our exercises below.

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

## A Parser that returns an ADT

The return type of the phone number parser above was `[Char]` (equivalent to `String`).  A more typical use case for a parser though is to generate some data structure that we can then process in other ways.  In Haskell, this usually means a parser which returns an [Algebraic Data Type (ADT)](/haskell2/algebraic-data-types).  Here is a very simple example.

Let's imagine we need to parse records from a vets office.  It treats only three types of animals.  As always, lets start with the BNF:

```
<Animal> :== "cat" | "dog" | "camel"
```

So our simple grammar consists of three terminals, each of which is a straight forward string *token* (a constant string that makes up a primitive word in our language).  To parse such a token, we'll need a parser which succeeds if it finds the specified string next in its input.  We'll use our `is` parser from above (which simply confirms a given character is next in its input).  The type of is was `Char -> Parser Char`.  Since `Parser` is an instance of `Applicative`, we can simply `traverse` the `is` parser across the given `String` (list of `Char`) to produce another `String` in the `Parser` applicative context.

```haskell
string :: String -> Parser String
string = traverse is
```

Now let's define an ADT for animals:

```haskell
data Animal = Cat | Dog | Camel
  deriving Show
```

A parser for "cat" is rather simple.  If we find the string `"cat"` we produce a `Cat`
```haskell
cat :: Parser Animal
cat = string "cat" >> pure Cat
```
Let's test it:
```haskell
> parse cat "cat"
Result >< Cat
```

Ditto dogs and camels:

```haskell
dog, camel :: Parser Animal
dog = string "dog" >> pure Cat
camel = string "camel" >> pure Cat
```

And now a parser for our full grammar:

```haskell
animal :: Parser Animal
animal = cat ||| dog ||| camel
```
Some tests:
```haskell
> parse animal "cat"
Result >< Cat
> parse animal "dog"
Result >< Dog
> parse animal "camel"
Result >< Camel
```
What's really cool about this is that obviously the strings "cat" and "camel" overlap at the start.  Our alternative parser `(|||)` effectively backtracks when the `cat` parser fails before eventually succeeding with the `camel` parser.  In an imperative style program this kind of logic would result in much messier code.

**Exercises**

- Modify the grammar and the ADT to have some extra data fields for each of the animal types, e.g. `humpCount`, `remainingLives`, `barkstyle`, etc.
- Make a parser `stringTok` which uses the `string` parser to parse a given string, but ignores any `spaces` before or after the token.
- Extend your parser to produce these records.

## Creating a Parse Tree

Programs are usually parsed into a tree structure called an *Abstract Syntax Tree* (AST), more generally known as a *parse tree*.  Further processing ultimately into an object file in the appropriate format (whether it's some sort of machine code directly executable on the machine architecture or some sort of intermediate format - e.g. Java bytecode) then essentially boils down to traversal of this tree to evaluate the statements and expressions there in the appropriate order.

We will not implement a parser for a full programming language, but to at least demonstrate what this concept looks like in Haskell we will create a simple parser for simple arithetic expressions.  The parser generates a tree structure capturing the order of operations, which we may then traverse to perform a calculation.

To start with, here is a BNF grammar for a simple calculator with three operations `*`, `+` and `-`, with `*` having higher precedence than `+` or `-`:

```
<expr> ::= <term> { <add> <term> }
<term> ::= <number> { "*" <number> }
<add> ::= "+" | "-"
```

An expression `<expr>` consists of one or more `<term>`s that may be combined with an `<add>` (either addition or subtraction).  A `<term>` involves one or more numbers, multiplied together.

The dependencies between the non-terminal expressions makes explicit the precedence of multiply operations needing to occur before add (and subtract).

The data structure we will create uses the following Algebraic Datatype:

```haskell
data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Number Integer
  deriving Show
```

Our top-level function will be called `parseCalc`:

```haskell
parseCalc :: String -> ParseResult Expr
parseCalc = parse expr
```

First difference to note from our `phoneNumber` is that instead of returning `::Parser [Char]`
And an example use might look like:

```haskell
> parseCalc " 6 *4 + 3- 8 *  2"
Result >< Minus (Plus (Times (Number 6) (Number 4)) (Number 3)) (Times (Number 8) (Number 2))
```

Here's some ASCII art to make the tree structure of the `ParseResult Expr` more clear:

```
Minus
 ├──Plus
 |   ├──Times
 |   |   ├──Number 6
 |   |   └──Number 4
 |   └──Number 3
 └──Times
     ├──Number 8
     └──Number 2
```

**Exercises**

- make an instance of `show` for `Expr` which pretty prints such trees
- Make a function which performs the calculation specified in an `Expr` tree like the one above.

Obviously we are going to need to parse numbers, so let's start with a simple parser which creates a `Number`.  
Note that whereas our previous parser had type `phoneNumber :: Parser [Char]` - i.e. it produced strings, this, and most of the parsers below produce an `Expr`.

```haskell
number :: Parser Expr  
number = spaces >> Number . read . (:[]) <$> digit
```

We keep things simple for now, make use of our existing `digit` parser, and limit our input to only single digit numbers.  
The expression `Number . read . (:[])` is fmapped over the `Parser Char` returned by `digit`.  
We use the Prelude function `read :: Read a => String -> a` to create the `Int` expected by `Number`.  Since `read` expects a string, we apply (:[]) to turn the `Char` into `[Char]`, i.e. a `String`.  

Next, we'll need a parser for the various operators (`*`,`+` and `-`).  There's enough of them that we'll make it a general purpose `Parser Char` parameterised by the character we expect:

```haskell
op :: Char -> Parser Char -- parse a single char operator
op c = do
   spaces
   is c
   pure c
```

As before, `spaces` ignores any number of `' '` characters.

Here's how we use `op` for `*`, note that it returns only the `Times` constructor.  Thus, our return type is an as-yet unapplied binary function (and we see now why `(<*>)` is going to be useful).

```haskell
times :: Parser (Expr -> Expr -> Expr)
times = op '*' >> pure Times
```

And for `+` and `-` a straightforward implementation of the `<add>` non-terminal from our grammar:

```haskell
add :: Parser (Expr -> Expr -> Expr)
add = (op '+' >> pure Plus) ||| (op '-' >> pure Minus)
```

And some more non-terminals:

```haskell
expr :: Parser Expr
expr = chain term add

term :: Parser Expr
term = chain number times
```

These use the `chain` function to handle repeated chains of operators (`*`, `-`, `+`) of unknown length.  We could make each of these functions recursive with a `|||` to provide an alternative for the base case end-of-chain (as we did for `spaces`, above), but we can factor the pattern out into a reusable function, like so:

```haskell
chain :: Parser a -> Parser (a->a->a) -> Parser a
chain p op = p >>= rest
   where
   rest a = (do
               f <- op
               b <- p
               rest (f a b)
            ) ||| pure a
```

**Exercises**

- Similar to `chain`, factor out the recursion of `spaces` into a function which returns a parser that continues producing a list of values from a given parser, i.e.
 `list :: Parser a -> Parser [a]`.