---
layout: page
title: "Introduction to Haskell"
permalink: /haskell1/
---


## Learning Outcomes


## Introduction

["Haskell Programming from First Principles” by Allen and Moronuki](http://haskellbook.com/) is a recent and excellent introduction to haskell that is quite compatible with the goals of this course.  The ebook is not too expensive, but unfortunately, it is independently published and hence not available from our library.  [”Learn you a Haskell” by Miran Lipovaca](http://learnyouahaskell.com/) is a freely available alternative that is also a useful introduction.

## Starting with the GHCi REPL
A good way to get started with haskell is simply to experiment with the GHCi REPL (Read Eval Print Loop).  

Start by making a file: `fibs.hs`
```haskell
fibs 0 = 1                       -- two base cases,
fibs 1 = 1                       -- resolved by pattern matching
fibs n = fibs (n-1) + fibs (n-2) -- recursive definition
```

Then load it into GHCi like so:
```haskell
$ stack ghci fibs.hs  
```
And now you can enter haskell expressions directly at the prompt:
```haskell
fibs 6
```
> 13  
Basic logic operators are similar to C/Java/etc: `==`, `&&`, `||`.  
```haskell
fibs 6 == 13
```
> True

An exception is not-equals, which is `/=` (Haskell tends to prefer more "mathy" syntax whenever possible).
```haskell
fibs 6 /= 13
```
> False

If-then-else expressions return a result (like javascript ternary ? :)
```haskell
if fibs 6 == 13 then "yes" else "no"
```
> "yes"

```haskell
if fibs 6 == 13 && fibs 7 == 12 then "yes" else "no"
```
>"no"

GHCi also has a number of non-haskell commands you can enter from the prompt, they are prefixed by "`:`".
You can reload your .hs file into ghci after an edit with `:r`.

## Creating a Runnable Haskell Program

Both the simplest and tail-recursive versions of our PureScript fibs code are also perfectly legal haskell code.  The main function will be a little different, however:

```haskell
main :: IO ()
main = print $ map fibs [1..10]
```

I’ve included the type signature for main although it’s not absolutely necessary (the compiler can usually infer the type of such functions automatically, as it did for our fibs function definition above), but it is good practice to define types for all top-level functions (functions that are not nested inside other functions) and also the IO type is interesting, and will be discussed at length later.  The main function takes no inputs (no need for -> with something on the left) and it returns something in the IO monad.  Without getting into it too much, yet, monads are special functions that can also wrap some other value.  In this case, the main function just does output, so there is no wrapped value and hence the () (called unit) indicates this.  You can think of it as being similar to the void type in C, Java or TypeScript.

What this tells us is that the main function produces an IO side effect.  This mechanism is what allows Haskell to be a pure functional programming language while still allowing you to get useful stuff done.  Side effects can happen, but when they do occur they must be neatly bundled up and declared to the type system, in this case through the IO monad.  For functions without side-effects, we have strong, compiler checked guarantees that this is indeed so (that they are pure).

By the way, once you are in the IO monad, you can’t easily get rid of it.  Any function that calls a function that returns an IO monad, must have IO as its return type.  Thus, effectful code is possible, but the type system ensures we are aware of it and can limit its taint.  The general strategy is to use pure functions wherever possible, and push the effectful code as high in your call hierarchy as possible.  Pure functions are much more easily reusable in different contexts.

The print function is equivalent to the PureScript log $ show.  Haskell also defines show for many types in the Prelude, but print in this case invokes it for us.  The other difference here is that square brackets operators are defined in the prelude for linked lists.  In PureScript they were used for Arrays - which (in PureScript) don’t have the range operator (..) defined so I avoided them.

Another thing to note about Haskell at this stage is that its evaluation is lazy by default.  Laziness is of course possible in other languages (as we have seen in JavaScript), and there are many lazy data-structures defined and available for PureScript (and most other functional languages).

However, lazy by default sets Haskell apart.  It has pros and cons, on the pro side:
It can make certain operations more efficient, for example, we have already seen in JavaScript how it can make streaming of large data efficient
It can enable infinite sequences to be defined and used efficiently (this is a significant semantic difference)
It opens up possibilities for the compiler to be really quite smart about its optimisations.
But there are definitely cons:
It can be hard to reason about run-time performance
Mixing up strict and lazy evaluation (which can happen inadvertently) can lead to (for example) O(n^2) behaviour in what should be linear time processing.

By the way, since it’s lazy-by-default, it’s possible to transfer the version of the Y-combinator given in the previous section into haskell code almost as given in Lambda Calculus:

```haskell
y = \f -> (\x -> f (x x)) (\x -> f (x x))
```

However, to get it to type-check one has to force the compiler to do some unsafe type coercion.  The following (along with versions of the Y-Combinator that do type check in haskell) are from an excellent Stack Overflow post:

```haskell
import Unsafe.Coerce
y :: (a -> a) -> a
y = \f -> (\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x))
main = putStrLn $ y ("circular reasoning works because " ++)
```
