---
layout: chapter
title: "Parser Combinators"
---

## Learning Outcomes

- Understand that a parser is a program which extracts information from structured text
- Apply what we have learned about Haskell typeclasses and other functional programming concepts to create solutions to real-world problems
- In particular, we learn to use parser combinators and see how they are put together

## Introduction

In this section we will see how the various Haskell language features we have explored allow us to solve real-world problems.  In particular, we will develop a simple but powerful library for building *parsers* that is compositional through [Functor](/haskell3/#functor), [Applicative](/haskell3/#applicative) and [Monad](/monad) interfaces.  Before this, though, we will learn the basics of parsing text, including a high-level understanding that parsers are *state-machines* which realise a *context-free grammar* over a textual language.

Previously, [we glimpsed a very simplistic Applicative parser](/haskell3/#a-simple-applicative-functor-for-parsing).
In this chapter, a *parser* is still simply a function which takes a string as input and produces some structure or computation as output, but now we extend the parser with monadic “bind” definitions, richer error handling and the ability to handle non-trivial grammars with alternative inputs.

Parsing has a long history and *parser combinators* are a relatively recent approach made popular by modern functional programming techniques.  
A *parser combinator* is a [higher-order function](/higherorderfunctions) that accepts parsers as input and combines them somehow into a new parser.

More traditional approaches to parsing typically involve special purpose programs called *parser generators*, which take as input a grammar defined in a special language (usually some derivation of BNF as described below) and generate the partial program in the desired programming language which must then be completed by the programmer to parse such input.  Parser combinators have the advantage that they are entirely written in the one language.  Parser combinators written in Haskell take advantage of the expressiveness of the Haskell language such that the finished parser can look a lot like a BNF grammar definition, as we shall see.

The parser combinator discussed here is based on one developed by Tony Morris and Mark Hibberd as part of their [“System F” Functional Programming Course](https://github.com/system-f/fp-course), which in turn is a simplified version of official Haskell parser combinators such as [parsec](https://hackage.haskell.org/package/parsec) by Daan Leijen.

You can play with the example and the various parser bits and pieces in [this on-line playground](https://replit.com/@tgdwyer/Parser-Examples).

## Context-free Grammars and BNF

Fundamental to analysis of human natural language but also to the design of programming languages is the idea of a *grammar*, or a set of rules for how elements of the language may be composed.  A context-free grammar (CFG) is one in which the set of rules for what is produced for a given input (*production rules*) completely cover the set of possible input symbols (i.e. there is no additional context required to parse the input).  Backus-Naur Form (or BNF) is a notation that has become standard for writing CFGs since the 1960s.  We will use BNF notation from now on.  There are two types of symbols in a CFG: *terminal* and *non-terminal*.  In BNF non-terminal symbols are `<nameInsideAngleBrackets>` and can be converted into a mixture of terminals and/or nonterminals by production rules:

```haskell
<nonterminal> ::= a mixture of terminals and <nonterminal>s, alternatives separated by |
```

Thus, *terminals* may only appear on the right-hand side of a production rule, *non-terminals* on either side.  In BNF each *non-terminal* symbol appears on the left-hand side of exactly one production rule, and there may be several possible alternatives for each *non-terminal* specified on the right-hand side.  These are separated by a `|` (in this regard they look a bit like the syntax for [algebraic data type definitions](/haskell2#algebraic-data-types)).

Note that production rules of the form above are for context-free grammars.  As a definition by counter-example, *context sensitive grammars* allow terminals and more than one non-terminal on the left hand side.

Here’s an example BNF grammar for parsing Australian land-line phone numbers, which may optionally include a two-digit area code in brackets, and then two groups of four digits, with an arbitrary number of spaces separating each of these, e.g.:

> (03) 9583 1762  
> 9583 1762

Here’s the BNF grammar:

```haskell
<phoneNumber> ::= <fullNumber> | <basicNumber>
<fullNumber> ::= <areaCode> <basicNumber>
<basicNumber> ::= <spaces> <fourDigits> <spaces> <fourDigits>
<fourDigits> ::= <digit> <digit> <digit> <digit>
<areaCode> ::= "(" <digit> <digit> ")"
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<spaces> ::= " " <spaces> | ""
```

So `"0"`-`"9"`, `"("`, `")"`, and `" "` are the full set of terminals.

Now here’s a sneak peak at a simple parser for such phone numbers.  It succeeds for any input string which satisfies the above grammar, returning a 10-digit string for the full number without spaces and assumes “03” for the area code for numbers with none specified (i.e. it assumes they are local to Victoria).  Our [`Parser` type](/parsercombinators/#parser-type) provides a function `parse` which we call like so:

```haskell
GHCi> parse phoneNumber "(02)9583 1762"
Result >< "0295831762"

GHCi> parse phoneNumber "9583  1762"
Result >< "0395831762"

GHCi> parse phoneNumber "9583-1762"
Unexpected character: "-"
```

We haven’t bothered to show the types for each of the functions in the code below, as they are all `::Parser [Char]` - meaning a Parser that returns a string.  We’ll explain all the types and functions used in due course.  For now, just notice how similar the code is to the BNF grammar definition:

```haskell
phoneNumber = fullNumber <|> (("03"++) <$> basicNumber)

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

In essence, our parser is going to be summed up by a couple of types:

```haskell
type Input = String
newtype Parser a = P { parse :: Input -> ParseResult a}
```

We assume all `Input` is a `String`, i.e. Haskell’s basic builtin `String` which is a list of `Char`.

Then the `Parser` type has one field `parse` which is a function of type `Input -> ParseResult a`.  So it parses strings and produces parse results, where a Parse result is:

```haskell
 data ParseResult a = Error ParseError
                    | Result Input a
  deriving Eq
```

We’ll come back to the `ParseError` type - which will be returned in the case of unexpected input, but we can see that a successful Parse is going to produce a `Result` which has two fields—more `Input` (the part of the input remaining after we took a bit off and parsed it), and an `a`, a type parameter that we may specify for concrete `Parser` instances.

The `Parser` and the `ParseResult` types are pretty abstract.  They say nothing about what precise `Input` string we are going to parse, or what type `a` we are going to return in the result.  This is the strength of the parser, allowing us to build up sophisticated parsers for different input grammars through composition using [instances of Function, Applicative and Monad](/parsercombinators/#instances), and the `ParseResult` parameter `a` allows us to produce whatever we want from the parsers we create.

## Error Handling

Error handling is a very important part of any real-world parser.  Decent error reporting allows us to quickly diagnose problems in our input.
As we saw above a `ParseResult` may be either a successful `Result` or an `Error`, the latter containing information in a `ParseError` data structure about the nature of the error.

```haskell
data ParseError =
    UnexpectedEof -- hit end of file when we expected more input
  | ExpectedEof Input -- should have successfully parsed everything but there’s more!
  | UnexpectedChar Char
  | UnexpectedString String
  deriving (Eq, Show)
```

Naturally it needs to be `Show`able, and we’ll throw in an `Eq` for good measure.
<!-- was “but also `Eq`uality testable so that we can pattern match `ParseResult` to handle particular types of errors.”. This may be misleading because pattern matching works even without Eq! -->

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

And `ParseResult` is also an instance of `Functor` so that we can map functions over the output of a successful parse—or do nothing if the result is an `Error`:

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

The applicative `pure` creates a `Parser` that always succeeds with the given input, and thus forms a basis for composition.  We saw it being used in the above example to return the results of a parse back into the `Parser` at the end of a `do` block.  

The `(<*>)` allows us to map functions in the `Parser` over another `Parser`.  As with other `Applicative` instances, a common use case would be composition with a `Parser` that returns a data constructor as we will see in [the next example](/parsercombinators/#creating-a-parse-tree).

```haskell
instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P (`Result` x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p q = p >>= (<$> q)
```

The `Monad` instance’s bind function `(>>=)` we have already seen in use in the example above, allowing us to sequence `Parser`s in `do`-blocks to build up the implementation of the BNF grammar.

```haskell
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (P p) f = P (
    \i -> case p i of
      Result rest x -> parse (f x) rest
      Error e -> Error e)
```

## Parser Combinators

The most atomic function for a parser of `String` is to pull a single character off the input.  The only thing that could go wrong is to find our input is empty.

```haskell
character :: Parser Char
character = P parseit
  where parseit "" = Error UnexpectedEof
        parseit (c:s) = Result s c
```

The following is how we will report an error when we encounter a character we didn’t expect.  This is not the logic for recognising a character, that’s already happened and failed and the unrecognised character is now the parameter.  This is just error reporting, and since we have to do it from within the context of a `Parser`, we create one using the `P` constructor.  Then we set up the one field common to any `Parser`, a function which returns a `ParseResult` no matter the input, hence `const`.  The rest creates the right type of `Error` for the given `Char`.

```haskell
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = P . const . Error . UnexpectedChar
```

Now a parser that insists on a certain character being the next one on the input.  It’s using the `Parser` instance of `Monad`’s bind function (implicitly in [a `do` block](/monad/#do-notation)) to sequence first the `character` `Parser`, then either return the correct character in the `Parser`, or the `Error` parser.

```haskell
is :: Char -> Parser Char
is c = do
  v <- character
  let next = if v == c
             then pure
             else const $ unexpectedCharParser v
  next c
```

And finally we introduce the `Alternative` typeclass for our `Parser` for trying to apply a first `Parser`, and then an alternate `Parser` if the first fails.  This allows us to encode the alternatives in our BNF grammar rules.

```haskell
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const (Error UnexpectedEof)

  p1 <|> p2 = P (\i -> let f (Error _) = parse p2 i
                         f r = r
                     in f $ parse p1 i)
```

## Nitty gritty

The last two pieces of our Phone Numbers grammar we also implement fairly straightforwardly from the BNF.  In a real parser combinator library you’d do it differently, as per our exercises below.

```haskell
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<spaces> ::= " " <spaces> | ""
```

Here’s a trivial adaptation of `digit`:

```haskell
digit :: Parser Char
digit = is '0' <|> is '1' <|> is '2' <|> is '3' <|> is '4' <|> is '5' <|> is '6' <|> is '7' <|> is '8' <|> is '9'
```

Spaces is a bit more interesting because it’s recursive, but still almost identical to the BNF:

```haskell
spaces :: Parser ()
spaces = (is ' ' >> spaces) <|> pure ()
```

---

### Exercises

- make a less repetitive `digit` parser by creating a function `satisfy :: (Char -> Bool) -> Parser Char` which returns a parser that produces a character but fails if the input is empty or the character does not satisfy the given predicate. You can use the `isDigit` function from `Data.Char` as the predicate.

- change the type of `spaces` to `Parser [Char]` and have it return the appropriately sized string of only spaces.

#### Solutions

We can generalise the `is` parser to handle a predicate

```haskell
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = do
  c <- character
  let next = if f c then pure else unexpectedCharParser
  next c
 

digit :: Parser Char
digit = satisfy isDigit
```

```haskell
spaces :: Parser [Char]
spaces = (do
    _ <- is ' '
    rest <- spaces
    pure (' ' : rest)
  ) <|> pure []
```

We can do this recursively, by trying to parse as many as possible, or we can use the many function to parse many spaces.

```haskell
spaces :: Parser [Char]
spaces = many (satisfy isSpace)
```

---

## A Parser that returns an ADT

The return type of the phone number parser above was `[Char]` (equivalent to `String`).  A more typical use case for a parser though is to generate some data structure that we can then process in other ways.  In Haskell, this usually means a parser which returns an [Algebraic Data Type (ADT)](/haskell2#algebraic-data-types).  Here is a very simple example.

Let’s imagine we need to parse records from a vets office.  It treats only three types of animals.  As always, lets start with the BNF:

```haskell
<Animal> ::= "cat" | "dog" | "camel"
```

So our simple grammar consists of three terminals, each of which is a straightforward string *token* (a constant string that makes up a primitive word in our language).  To parse such a token, we’ll need a parser which succeeds if it finds the specified string next in its input.  We’ll use our `is` parser from above (which simply confirms a given character is next in its input).  The type of is was `Char -> Parser Char`.  Since `Parser` is an instance of `Applicative`, we can simply `traverse` the `is` parser across the given `String` (list of `Char`) to produce another `String` in the `Parser` applicative context.

```haskell
string :: String -> Parser String
string = traverse is
```

Now let’s define an ADT for animals:

```haskell
data Animal = Cat | Dog | Camel
  deriving Show
```

A parser for “cat” is rather simple.  If we find the string `"cat"` we produce a `Cat`:

```haskell
cat :: Parser Animal
cat = string "cat" >> pure Cat
```

Let’s test it:

```haskell
> parse cat "cat"
Result >< Cat
```

Ditto dogs and camels:

```haskell
dog, camel :: Parser Animal
dog = string "dog" >> pure Dog
camel = string "camel" >> pure Camel
```

And now a parser for our full grammar:

```haskell
animal :: Parser Animal
animal = cat <|> dog <|> camel
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

What’s really cool about this is that obviously the strings “cat” and “camel” overlap at the start.  Our alternative parser `(<|>)` effectively backtracks when the `cat` parser fails before eventually succeeding with the `camel` parser.  In an imperative style program this kind of logic would result in much messier code.

---

## Exercises

- Make a parser `stringTok` which uses the `string` parser to parse a given string, but ignores any `spaces` before or after the token.
- Write some messy imperative-style JavaScript (no higher-order functions allowed) to parse cat, dog or camel and construct a different class instance for each.
- Now add “dolphin” to the grammar, and use a stopwatch to time yourself extending your messy imperative code.  I bet it takes longer than extending the `animal` parser combinator.
- Modify the grammar and the ADT to have some extra data fields for each of the animal types, e.g. `humpCount`, `remainingLives`, `barkstyle`, etc.
- Extend your parser to produce these records.

#### Solutions

- To create `stringTok`, we can make use of `<<` or `>>` to ignore parts of the result:

```haskell
stringTok :: String -> Parser String
stringTok s = spaces >> string s << spaces
```

- Messy imperative JavaScript to parse animals and construct appropriate class instances:

```javascript
// Define the classes for Cat, Dog, and Camel
class Cat {
  constructor() {
    this.type = 'Cat';
  }
}

class Dog {
  constructor() {
    this.type = 'Dog';
  }
}

class Camel {
  constructor() {
    this.type = 'Camel';
  }
}

class Dolphin {
  constructor() {
    this.type = 'Dolphin';
  }
}

// Imperative parser function
function parseAnimal(input) {
  let animal = null;

  if (input === 'cat') {
    animal = new Cat();
  } else if (input === 'dog') {
    animal = new Dog();
  } else if (input === 'dolphin') {
    animal = new Dolphin();
  } else if (input === 'camel') {
    animal = new Camel();
  } else {
    throw new Error('Invalid input');
  }

  return animal;
}

// Example usage
try {
  const animal1 = parseAnimal('cat');
  console.log(animal1); // Cat { type: 'Cat' }

  const animal2 = parseAnimal('dog');
  console.log(animal2);
}
```

---

## Creating a Parse Tree

Programs are usually parsed into a tree structure called an *Abstract Syntax Tree* (AST), more generally known as a *parse tree*.  Further processing ultimately into an object file in the appropriate format (whether it’s some sort of machine code directly executable on the machine architecture or some sort of intermediate format—e.g. Java bytecode) then essentially boils down to traversal of this tree to evaluate the statements and expressions there in the appropriate order.

We will not implement a parser for a full programming language, but to at least demonstrate what this concept looks like in Haskell we will create a simple parser for simple arithmetic expressions.  The parser generates a tree structure capturing the order of operations, which we may then traverse to perform a calculation.

To start with, here is a BNF grammar for a simple calculator with three operations `*`, `+` and `-`, with `*` having higher precedence than `+` or `-`:

```lambdacalc
<expr> ::= <term> | <expr> <addop> <term>
<term> ::= <number> | <number> "*" <number>
<addop> ::= "+" | "-"
```

An expression `<expr>` consists of one or more `<term>`s that may be combined with an `<addop>` (an addition operation, either `"+"` or `"-"`).  A `<term>` involves one or more numbers, multiplied together.

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

And an example use might look like:

```haskell
> parseCalc " 6 *4 + 3- 8 *  2"
Result >< Minus (Plus (Times (Number 6) (Number 4)) (Number 3)) (Times (Number 8) (Number 2))
```

Here’s some ASCII art to make the tree structure of the `ParseResult Expr` more clear:

```lambdacalc
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

---

### Exercises

- make an instance of `show` for `Expr` which pretty prints such trees
- Make a function which performs the calculation specified in an `Expr` tree like the one above.

---

Obviously we are going to need to parse numbers, so let’s start with a simple parser which creates a `Number`.  
Note that whereas our previous parser had type `phoneNumber :: Parser [Char]`—i.e. it produced strings—this, and most of the parsers below, produces an `Expr`.

```haskell
number :: Parser Expr
number = spaces >> Number . read . (:[]) <$> digit
```

We keep things simple for now, make use of our existing `digit` parser, and limit our input to only single digit numbers.  
The expression `Number . read . (:[])` is fmapped over the `Parser Char` returned by `digit`.  
We use the Prelude function `read :: Read a => String -> a` to create the `Int` expected by `Number`.  Since `read` expects a string, we apply `(:[])` to turn the `Char` into `[Char]`, i.e. a `String`.  

Next, we’ll need a parser for the various operators (`*`,`+` and `-`).  There’s enough of them that we’ll make it a general purpose `Parser Char` parameterised by the character we expect:

```haskell
op :: Char -> Parser Char -- parse a single char operator
op c = do
   spaces
   is c
   pure c
```

As before, `spaces` ignores any number of `' '` characters.

Here’s how we use `op` for `*`; note that it returns only the `Times` constructor.  Thus, our return type is an as-yet unapplied binary function (and we see now why `(<*>)` is going to be useful).

```haskell
times :: Parser (Expr -> Expr -> Expr)
times = op '*' >> pure Times
```

And for `+` and `-` a straightforward implementation of the `<addop>` non-terminal from our grammar:

```haskell
addop :: Parser (Expr -> Expr -> Expr)
addop = (op '+' >> pure Plus) <|> (op '-' >> pure Minus)
```

And some more non-terminals:

```haskell
expr :: Parser Expr
expr = chain term addop

term :: Parser Expr
term = chain number times
```

These use the `chain` function to handle repeated chains of operators (`*`, `-`, `+`) of unknown length.  We could make each of these functions recursive with a `<|>` to provide an alternative for the base case end-of-chain (as we did for `spaces`, above), but we can factor the pattern out into a reusable function, like so:

```haskell
chain :: Parser a -> Parser (a->a->a) -> Parser a
chain p op = p >>= rest
   where
   rest :: a -> Parser a
   rest a = (do
               f <- op
               b <- p
               rest (f a b)
            ) <|> pure a
```

But, how does `chain` work?

`p >>= rest`: The parser `p` is applied, and we pass this parsed value, to the function call `rest`

`rest a`: Within the rest function, the parser op is applied to parse an operator `f`, and the parser `p` is applied again to parse another value `b`. The result is then combined using the function `f` applied to both `a` and `b` to form a new value. The rest function is then called recursively, with this new value.

Recursive calls: The recursive calls continue until there are no more operators `op` to parse, at which point the parser returns the last value `a`. This is achieved using the `pure a` expression. This makes the function *tail recursive*

This gives us a way to parse expressions of the form “1+2+3+4+5” by parsing “1” initially, using `p` then repeatedly parsing something of the form “+2”, where `op` would parse the “+” and the `p` would then parse the “2”. These are combined using our `Plus` constructor to be of form `Plus 1 2`, this will then recessively apply the `p` and `op` parser over the rest of the string: “+3+4+5”.

But, can we re-write this using a fold?

```haskell
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = foldl applyOp <$> p <*> many (liftA2 (,) op p)
  where
    applyOp :: a -> (a->a->a, a) -> a
    applyOp x (op, y) = op x y
```

`foldl applyOp <$> p`: This part uses the Functor instances to combine the parsed values and apply the operators in a left-associative manner. `foldl applyOp` is partially applied to `p`, creating a parser that parses an initial value (`p`) and then applies the left-associative chain of operators and values.

`many ((,) <$> op <*> p)`: This part represents the repetition of pairs (op, p) using the many combinator. The tuple structure here just allows us to store the pairs of `op` and `p`. We use liftA2 to lift both parse results in to the tuple constructor. We run this many times, to parse many pairs of `op` and `p`, and create a list of tuples. As a result, it creates a parser that parses an operator (op) followed by a value (p) and repeats this zero or more times.

`applyOp x (op, y)`: This function is used by `foldl` to combine the parsed values and operators. It takes an accumulated value `x`, an operator `op`, and a new value `y`, and applies the operator to the accumulated value and the new value.

---

### Exercises

- Similar to `chain`, factor out the recursion of `spaces` into a function which returns a parser that continues producing a list of values from a given parser, i.e.
 `list :: Parser a -> Parser [a]`.

---

## Parsing Rock-Paper-Scissors

A common use-case for parsing is deserialising data stored as a string.
Of course, there are general data interchange formats such as JSON and XML for which most languages have parsers available.  However, sometimes you want to store data in your own format for compactness or readability, and when you do, deserialising the data requires a custom parser
(this example is contributed by [Arthur Maheo](https://arthur.maheo.net/)).

We will explore a small game of Rock-Paper-Scissors using a memory.
The play function will have the following type:

``` haskell
data RockPaperScissors = Rock | Paper | Scissors

-- | Play a round of RPS given the result of the previous round.
play
  :: Maybe (RockPaperScissors, RockPaperScissors, String)
  -- ^ Result of the previous round as: (your choice, opponent choice, your memory)
  -> (RockPaperScissors, String) -- ^ (Choice, new memory)
```

We will build a simple player which will keep track of the opponent’s previous choices and try to counter the most common one.

### How to build a memory

We will convert to string using a simple `Show` instance:

``` haskell
instance Show RockPaperScissors where
  show :: RockPaperScissors -> String
  show Rock = "R"
  show Paper = "P"
  show Scissors = "S"
```

(Note, we could also define a `Read` instance to deserialise such a simple type but we are going to define a `ParserCombinator` for interest and extensibility to much more complex scenarios).

The straightforward way to create the memory is to just store a list of all the choices made by the opponent.
So, for example, if the results from the previous three rounds were:

``` haskell
(Rock, Paper), (Rock, Scissors), (Paper, Scissors)
```

Then, a compact memory representation will be: `"PSS"`.

*Note*: We only store single characters, so we do not need separators, but if you have more complex data, you will want separators.

### Reading the memory

Now, we want to define a `Parser RockPaperScissors` which will turn a string into a choice.
First, we will define a parser for each of the three choices:

``` haskell
rock :: Parser RockPaperScissors
rock = is 'R' >> pure Rock

scissors :: Parser RockPaperScissors
scissors = is 'S' >> pure Scissors

paper :: Parser RockPaperScissors
paper = is 'P' >> pure Paper
```

This will give:

```haskell
>>> parse rock "R"
Result >< R
>>> parse rock "RR"
Result >R< R
>>> parse rock "P"
Unexpected character: "P"
```

To combine those parsers, we will use the *option parser* `(<|>)`.

```haskell
choice :: Parser RockPaperScissors
choice = rock <|> paper <|> scissors
```

And, to be able to read a list of choices, we need to use the `list` parser:

```haskell
>>> parse choice "PSS"
Result >SS< P
>>> parse (list choice) "PSCS"
Result >CS< [P,S]
>>> parse (list choice) "PSS"
Result >< [P,S,S]
```

### Playing the game

Our decision function will take a list of `RockPaperScissors` and return the move that would win against most of them.
One question remains: how do we get the memory *out* of the parser?
The answer is: pattern-matching.

``` haskell
getMem :: ParseResult a -> a
getMem (Result _ cs) = cs
getMem (Error _) = error "You should not do that!"
```

Obviously, in a longer program you want to be handling this case better.

*Hint*: If your parser returns a list of elements, the empty list `[]` is a good default case.

### Putting it all together

The first round, our player will just pick a choice at random and return an empty memory.

``` haskell
play Nothing = (Scissors, "") -- Chosen at random!
```

Now, we need to write a couple functions:

1. `winAgainst` that determines which choice wins against a given one.
2. `mostCommon` which finds the most common occurrence in a list.

With that, we have a full `play` function:

``` haskell
play (Just (_, opponent, mem)) = (winning whole, concatMap convert whole)
  where
    -- Convert the memory to a list of different choices
    as_choices = getMem . parse (list choice)
    -- Get the whole set of moves—all the prev. rounds + last one
    whole = opponent: as_choices mem
    winning = winAgainst . mostCommon
```

``` haskell
>>> play Nothing
(S,"")
>>> play (Just (Scissors, Scissors, ""))
(R,"S")
>>> play (Just (Scissors, Scissors, "RRP"))
(P,"SRRP")
```

*Note*: Here we can see the results directly because `RockPaperScissors` has an instance of `Show`.
If you want to do the same with a datatype without `Show`, you would need to call `convert`.

### Going further

Now, this is a simplistic view of storing information.
We are only concatenating characters because our data is so small.
However, there are better ways to store that data.

One issue with this approach is that we need to process the memory sequentially at each round.
Instead, we could keep track of the number of occurrences of each choice.

---

### Exercise

Implement a memory for the following datatype.

``` haskell
data Played = Played {rocks, papers, scissors :: Int}

-- | Store a @Played@ as a string in format: @nC@, with @n@ the number of
-- occurrences and @C@ the choice.
convert' :: Played -> String
convert' Played{rocks, papers, scissors} = 
  show rocks ++ "R" ++ show papers ++ "P" ++ show scissors ++ "S"
```

```haskell
>>> play Nothing
(S,"0R0P0S")
>>> play (Just (Scissors, Scissors, "0R0P0S"))
(R,"0R0P1S")
>>> play (Just (Scissors, Scissors, "2R1P0S"))
(P,"2R1P1S")
```

---

## Glossary

*Parser*: A program that processes a string of text to extract structured information from it. Parsers are used in interpreting programming languages, data formats, and other structured text formats.

*Context-Free Grammar*: A type of formal grammar that is used to define the syntax of programming languages and data formats. CFGs consist of a set of production rules that define how terminals and non-terminals can be combined to produce strings in the language.

*Backus-Naur Form*: A notation for expressing context-free grammars. It is used to formally describe the syntax of programming languages.

*Terminal*: In the context of grammars, a terminal is a symbol that appears in the strings generated by the grammar. Terminals are the actual characters or tokens of the language.

*Non-Terminal*: A symbol in a grammar that can be replaced by a sequence of terminals and non-terminals according to the production rules of the grammar.

*Parser Combinator*: A higher-order function that takes parsers as input and combines them to create new parsers. Parser combinators are used to build complex parsers in a modular and compositional way.

*Abstract Syntax Tree*: A tree representation of the abstract syntactic structure of a string of text. Each node in the tree represents a construct occurring in the text.
