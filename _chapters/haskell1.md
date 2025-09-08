---
layout: chapter
title: "Creating and Running Haskell Programs"
---

## Learning Outcomes

- Use the GHCi REPL to test Haskell programs and expressions
- Compare the syntax of Haskell programs to Functional-style code in JavaScript
- Understand that by default Haskell uses a lazy evaluation strategy
- Create and use Haskell lists and tuples
- Create Haskell functions using *pattern-matching*, *guards*, and local definitions using `where` and `let` clauses.

## Introduction

This section is not your usual “First Introduction To Haskell” because it assumes you have arrived here having already studied some reasonably sophisticated functional programming concepts in JavaScript, basic parametric polymorphic types in [TypeScript](/typescript/), and [Lambda Calculus](/lambdacalculus).  Familiarity with [higher-order](/higherorderfunctions/) and [curried functions](/functionaljavascript/) is assumed.  

I try to summarise the syntax we use with “cheatsheets” throughout, but [the official CheatSheet](https://hackage.haskell.org/package/CheatSheet-1.10/src/CheatSheet.pdf) will be a useful reference.  The other reference every Haskell programmer needs is the official library docs on [Hackage](https://hackage.haskell.org/) which is searchable by the excellent [Hoogle](https://hoogle.haskell.org/) search engine, which lets you search by types as well as function keywords.

If you would like a more gradual introduction, [“Haskell Programming from First Principles” by Allen and Moronuki](http://haskellbook.com/) is a recent and excellent introduction to Haskell that is quite compatible with the goals of this course.  The ebook is not too expensive, but unfortunately, it is independently published and hence not available from our library.  There are a few copies of [Programming Haskell by Hutton](https://monash.hosted.exlibrisgroup.com/permalink/f/31uhmh/catau21316253770001751) which is an excellent academic textbook, but it’s expensive to buy.  [“Learn you a Haskell” by Miran Lipovaca](https://learnyouahaskell.github.io/) is a freely available alternative that is also a useful introduction.  

## Starting with the GHCi REPL

A good way to get started with Haskell is simply to experiment with the GHCi REPL (or Read Eval Print Loop).

Start by making a file: `fibs.hs`

```haskell
fibs 0 = 1                       -- two base cases,
fibs 1 = 1                       -- resolved by pattern matching
fibs n = fibs (n-1) + fibs (n-2) -- recursive definition
```

Then load it into GHCi like so:

```bash
stack ghci fibs.hs
```

<div class="alert-box alert-warning" markdown="1">
**`stack ghci` vs `ghci`**

If you followed our [Haskell installation instructions](/haskell0), you will not have GHC installed globally, and will need to run `stack ghci` instead of just `ghci` on its own. This should work if you run the command from inside the folder for the applied exercises. If you run it outside of the folder, you might get this message:

```none
Writing the configuration file for the implicit global project to:
/Users/username/.stack/global-project/stack.yaml. Note: You can change the snapshot via
the snapshot key there.
Using the latest snapshot lts-24.7.

Error: [S-6362]
       No compiler found, expected minor version match with ghc-9.10.2 (aarch64) (based
       on the configuration in /Users/username/.stack/global-project/stack.yaml).

       To install the correct version of GHC into the subdirectory for the specified
       platform in Stack's directory for local tools
       (/Users/username/.stack/programs/aarch64-osx/), try running stack setup or use
       the --install-ghc flag. To use your system GHC installation, run stack config set
       system-ghc --global true, or use the --system-ghc flag.
```

To fix this, run

```sh
stack config set snapshot lts-23.25
```

and then run `stack setup`.

If you are working in an Ed workspace (e.g. for workshops), Stack is not installed, so you will need to run `ghci` instead of `stack ghci`.
</div>

You’ll get a prompt that looks like:

```haskell
ghci>
```

You can enter Haskell expressions directly at the prompt:

```haskell
ghci> fibs 6
```

> 13

I’m going to stop showing the prompt now, but you can enter all of the following directly at the prompt and you will see similar results printed to those indicated below.

Basic logic operators are similar to C/Java/etc: `==`, `&&`, `||`.

```haskell
fibs 6 == 13
```

> True

An exception is “not-equal”, whose operator is `/=` (Haskell tends to prefer more “mathy” syntax whenever possible).

```haskell
fibs 6 /= 13
```

> False

If-then-else expressions return a result (like JavaScript’s ternary `? :`)

```haskell
if fibs 6 == 13 then "yes" else "no"
```

> "yes"

```haskell
if fibs 6 == 13 && fibs 7 == 12 then "yes" else "no"
```

>"no"

GHCi also has a number of non-Haskell commands you can enter from the prompt, which are prefixed by `:`.
You can reload your .hs file into GHCi after an edit with `:r`.  Type `:h` for help.

You can declare local variables in the REPL:

```haskell
x = 3
y = 4
x + y
```

> 7

One of Haskell’s distinguishing features from other languages is that its variables are strictly immutable (i.e. not variable at all really).  This is similar to variables declared with JavaScript’s `const` keyword - but everything in Haskell is deeply immutable by default.

However, in the GHCi REPL, you can redefine variables and Haskell will not complain.

## Creating a Runnable Haskell Program

Both the simplest and tail-recursive versions of our PureScript fibs code are also perfectly legal Haskell code.  The main function will be a little different, however:

```haskell
main :: IO ()
main = print $ map fibs [1..10]
```

I’ve included the type signature for main although it’s not absolutely necessary (the compiler can usually infer the type of such functions automatically, as it did for our fibs function definition above), but it is good practice to define types for all top-level functions (functions that are not nested inside other functions) and also the `IO` type is interesting, and will be discussed at length later.  The main function takes no inputs (no need for `->` with something on the left) and it returns something in the `IO` monad.  Without getting into it too much, yet, monads are special types that can also wrap some other value.  In this case, the main function just does output, so there is no wrapped value and hence the `()` (called unit) indicates this.  You can think of it as being similar to the void type in C, Java or TypeScript.

What this tells us is that the main function produces an IO side effect.  This mechanism is what allows Haskell to be a pure functional programming language while still allowing you to get useful stuff done.  Side effects can happen, but when they do occur they must be neatly bundled up and declared to the type system, in this case through the `IO` monad.  For functions without side effects, we have strong, compiler-checked guarantees that this is indeed so (that they are *pure*).

By the way, once you are in the `IO` monad, you can’t easily get rid of it.  Any function that calls a function that returns an `IO` monad, must have `IO` in its return type.  Thus, effectful code is possible, but the type system ensures we are aware of it and can limit its taint.  The general strategy is to use pure functions wherever possible, and push the effectful code as high in your call hierarchy as possible—that is, limit the size and scope of impure code as much as possible.  Pure functions are much more easily reusable in different contexts.

The `print` function is equivalent to the PureScript `log $ show`.  That is, it uses any available `show` function for the type of value being printed to convert it to a string, and it then prints that string.  Haskell defines show for many types in the Prelude, but print in this case invokes it for us.  The other difference here is that square brackets operators are defined in the prelude for linked lists.  In PureScript they were used for Arrays---which (in PureScript) don’t have the range operator (`..`) defined so I avoided them.  Speaking of List operators, here’s a summary:

<div class="cheatsheet" markdown="1">

## Basic List and Tuple Operator Cheatsheet

The default Haskell lists are cons lists (linked lists defined with a `cons` function), similar to [those we defined in JavaScript](/functionaljavascript/#computation-with-pure-functions).

```haskell
[]           -- an empty list
[1,2,3,4]    -- a simple lists of values
[1..4]       -- ==[1,2,3,4] (..) is range operator
1:[2,3,4]    -- ==[1,2,3,4], use `:` to “cons” an element to the start of a list
1:2:3:[4]    -- ==[1,2,3,4], you can chain `:`
[1,2]++[3,4] -- ==[1,2,3,4], i.e. (++) is concat

-- You can use `:` to pattern match lists in function definitions.
-- Note the enclosing `()` to delimit the pattern for the parameter.
length [] = 0
length (x:xs) = 1 + length xs -- x is bound to the head of the list and xs the tail
-- (although you don’t need to define `length`, it’s already loaded by the prelude)

length [1,2,3]
```

> 3

Some other useful functions for dealing with lists:

```haskell
head [1,2,3] -- 1
tail [1,2,3] -- [2,3]

sum [1,2,3] -- 6 (but only applicable for lists of things that can be summed)
minimum [1,2,3] -- 1 (but only for Ordinal types)
maximum [1,2,3] -- 3

map f [1,2,3] -- maps the function f over the elements of the list returning the result in another list
```

Tuples are fixed-length collections of values that may not necessarily be of the same type.  They are enclosed in `()`

```haskell
t = (1,"hello") -- define variable t to a tuple of an Int and a String.
fst t
```

> 1

```haskell
snd t
```

> "hello"

And you can destructure and pattern match tuples:

```haskell
(a,b) = t
a
```

> 1

```haskell
b
```

> "hello"

Note that we created tuples in JavaScript using `[]`—actually they were fixed-length arrays, don’t confuse them for Haskell lists or tuples.
</div>

## Lazy by Default

Haskell's strategy for evaluating expressions is lazy by default—that is it defers evaluation of expressions until it absolutely must produce a value.  Laziness is of course possible in other languages ([as we have seen in JavaScript](/lazyevaluation/)), and there are many lazy data structures defined and available for PureScript (and most other functional languages).
Conversely, Haskell can be [forced to use strict evaluation](https://wiki.haskell.org/Performance/Strictness) and has libraries of data structures with strict semantics if you need them.

However, lazy by default sets Haskell apart.  It has pros and cons; on the pro side:

- It can make certain operations more efficient, for example, we have already seen in JavaScript how it can make streaming of large data efficient
- It can enable infinite sequences to be defined and used efficiently (this is a significant semantic difference)
- It opens up possibilities for the compiler to be really quite smart about its optimisations.

But there are definitely cons:

- It can be hard to reason about runtime performance
- Mixing up strict and lazy evaluation (which can happen inadvertently) can lead to (for example) O(n<sup>2</sup>) behaviour in what should be linear time processing.

## Lazy infinite lists

Note that our "Hello world!" function to recursively compute the $n^{th}$ Fibonacci number [above](#starting-with-the-ghci-repl) was not at all efficient (in fact $O(2^n)$).  We will now demonstrate a very idiomatic haskell construction for defining a lazy sequence of Fibonacci numbers that is linear time in the number of fibs required.  In the following definition for `lazyFibs`, `zipWith` is a function which uses the specified function (in this case `(+)`) to pair the heads of two given lists.  In this case, we are zipping over recursive references to `lazyFibs` and `tail lazyFibs`.

```haskell
lazyFibs = 1 : 1 : zipWith (+) lazyFibs (tail lazyFibs)
```
We can then create as much of the list as we need:
```haskell
take 10 lazyFibs
[1,1,2,3,5,8,13,21,34,55]
```
This is only possible because Haskell's lazy evaluation only forces evaluation of the heads of the lists as necessary, e.g. to output the result of `take 10`.
![Deck Observable Visualised](/assets/images/chapterImages/haskell1/zip.gif)

<div class="alert-box alert-info" markdown="1">
**A Side Note on the Y Combinator**

The Haskell way of defining Lambda (anonymous) functions is heavily inspired by [Lambda Calculus](/lambdacalculus/), but also looks a bit reminiscent of the JavaScript arrow syntax:

```none
Lambda Calculus
λx. x

JavaScript
x => x

Haskell
\x -> x
```

Since it’s lazy-by-default, it’s possible to transfer the version of the [Y-combinator we explored in Lambda Calculus](/lambdacalculus/#divergent-lambda-expressions) into Haskell code almost as it appears in Lambda Calculus:

```haskell
y = \f -> (\x -> f (x x)) (\x -> f (x x))
```

However, to get it to type-check, one has to either write some gnarly type definitions or force the compiler to do some unsafe type coercion.  The following (along with versions of the Y-Combinator that do type check in Haskell) are from an excellent [Stack Overflow post](https://stackoverflow.com/questions/4273413/y-combinator-in-haskell):

```haskell
import Unsafe.Coerce
y :: (a -> a) -> a
y = \f -> (\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x))
main = putStrLn $ y ("circular reasoning works because " ++)
```

</div>

## Functional Programming in Haskell versus JavaScript

Consider the following pseudocode for a simple recursive definition of the Quick Sort algorithm:

```none
QuickSort list:
  Take head of list as a pivot
  Take tail of list as rest
  return
QuickSort( elements of rest < pivot ) ++ (pivot : QuickSort( elements of rest >= pivot ))
```

We’ve added a bit of notation here: `a : l` inserts a (“cons”es) to the front of a list `l` ; `l1 ++ l2` is the concatenation of lists `l1` and `l2`.

In JavaScript the fact that we have anonymous functions through compact arrow syntax and expression syntax if (with `? :`) means that we can write pure functions that implement this recursive algorithm in a very functional, fully-curried style.  However, the language syntax really doesn’t do us any favours!

For example,

```javascript
const
  sort = lessThan=>
    list=> !list ? null :
      (pivot=>rest=>
        (lesser=>greater=>
          concat(sort(lessThan)(lesser))
                (cons(pivot)(sort(lessThan)(greater)))
        )(filter(a=> lessThan(a)(pivot))(rest))
         (filter(a=> !lessThan(a)(pivot))(rest))
      )(head(list))(tail(list))
```

Consider the following, more-or-less equivalent Haskell implementation:

```haskell
sort [] = []
sort (pivot:rest) = lesser ++ [pivot] ++ greater
  where
    lesser = sort $ filter (<pivot) rest
    greater = sort $ filter (>=pivot) rest
```

An essential thing to know before trying to type in the above function is that Haskell delimits the scope of multi-line function definitions (and all multiline expressions) with indentation ([complete indentation rules reference here](https://en.wikibooks.org/wiki/Haskell/Indentation)). The `where` keyword lets us create multiple function definitions that are visible within the scope of the parent function, but they must all be left-aligned with each other and to the right of the start of the line containing the `where` keyword.

Haskell also helps with a number of other language features.  
The first is pattern matching.  Pattern matching is like function overloading that you may be familiar with from languages like Java or C++ - where the compiler matches the version of the function to invoke for a given call by matching the type of the parameters to the type of the call - except in Haskell the compiler goes a bit deeper to inspect the values of the parameters.

There are two declarations of the sort function above.  The first handles the base case of an empty list.  The second handles the general case, and pattern matching is again used to destructure the lead cons expression into the pivot and rest variables.  No explicit call to head and tail functions is required.

The next big difference between our Haskell quicksort and our previous JavaScript definition is the Haskell style of function application---which has more in common with lambda calculus than JavaScript.  The expression `f x` is application of the function `f` to whatever `x` is.

Another thing that helps with readability is infix operators.  For example, `++` is an infix binary operator for list concatenation. The `:` operator for cons is another.  There is also the aforementioned `$` which gives us another trick for removing brackets, and finally, the `<` and `>=` operators.  Note that infix operators can also be curried and left only partially applied as in `(<pivot)`.

Next, we have the `where` which lets us create locally scoped variables within the function declaration without the need for the trick I used in the JavaScript version of using the parameters of anonymous functions as locally scoped variables.

Finally, you’ll notice that the Haskell version of `sort` appears to be missing a parameterisation of the order function.  Does this mean it is limited to number types?  In fact, no---from our use of `<` and `>=`, the compiler has inferred that it is applicable to any ordered type.  More specifically, to any type in the type class `Ord`.

I deliberately avoided the type declaration for the above function because: (1) we haven’t really talked about types properly yet, and (2) I wanted to show off how clever Haskell type inference is.  However, it is actually good practice to include the type signature.  If one were to load the above code, without type definition, into GHCi (the Haskell REPL), one could interrogate the type like so:

```haskell
> :t sort
sort :: Ord t => [t] -> [t]
```

Thus, the function `sort` has a generic type parameter `t` (we’ll talk more about such [parametric polymorphism in haskell](/haskell2/#type-parameters-and-polymorphism) later) which is constrained to be in the `Ord` type class (anything that is orderable - we’ll talk more about [type classes](/haskell2/#typeclasses) too).  Its input parameter is a list of `t`, as is its return type.  This is also precisely the syntax that one would use to declare the type explicitly.  Usually, for all top-level functions in a Haskell file, it is good practice to explicitly give the type declaration.  Although it is not always necessary, it can avoid ambiguity in many situations and once you get good at reading Haskell types it becomes useful documentation.

Here’s another refactoring of the quick-sort code.  This time with type declaration because I just said it was the right thing to do:

```haskell
sort :: Ord t => [t] -> [t]
sort [] = []
sort (pivot:rest) = below pivot rest ++ [pivot] ++ above pivot rest
 where
   below p = partition (<p)
   above p = partition (>=p)
   partition comparison = sort . filter comparison
```

The `list` parameter for `below` and `above` has been eta-reduced away just as we were able to [eta-reduce lambda calculus expressions](/lambdacalculus/#lambda-calculus-cheatsheet).  The definition of the `partition` function in this version uses the `.` operator for [function composition](/higherorderfunctions/#composition).  That is, `partition comparison` is the composition of `sort` and `filter comparison` and again the `list` parameter is eta-reduced away.

Although it looks like the comparison parameter could also go away here with eta conversion, actually the low precedence of the `.` operator means there is (effectively) implicit parentheses around filter comparison.  We will see how to [more aggressively refactor code to be point-free later](/haskell3/#point-free-code).

The idea of refactoring our code into the above form was to demonstrate the freedom that Haskell gives us to express logic
in a way that makes sense to us.  This version reads almost like a natural language declarative definition of the algorithm.  That is, you can read:

```haskell
sort (pivot:rest) = below pivot rest ++ [pivot] ++ above pivot rest
```

as:
> the sort of a list where we take the first element as the “pivot” and everything after as “rest” is
> everything that is below pivot in rest,  
> concatenated with a list containing just the pivot,  
> concatenated with everything that is above pivot in rest.

Haskell has a number of features that allow us to express ourselves in different ways.  Above we used a `where` clause to give a post-hoc, locally-scoped declaration of the below and above functions.  Alternately, we could define them at the start of the function body with `let <variable declaration expression> in <body>`.  Or we can use `let`, `in` and `where` all together, like so:

```haskell
sort :: Ord t => [t] -> [t]
sort [] = []
sort (pivot:rest) = let
   below p = partition (<p)
   above p = partition (>=p)
 in
   below pivot rest ++ [pivot] ++ above pivot rest
 where
   partition comparison = sort . filter comparison
```

Note that where is only available in function declarations, not inside expressions and therefore is not available in a lambda.  However, `let`-`in` is part of the expression, and therefore available inside a lambda function.  A silly example would be:  `\i -> let f x = 2*x in f i`, which could also be spread across lines, but be careful to get the correct indentation.

<div class="cheatsheet" markdown="1">
**Conditional Code Constructs Cheatsheet**

### Pattern matching

Provides alternative cases for function definitions matching different values or possible destructurings of the function arguments ([more detail](/haskell2#pattern-matching)).  As per examples above and:

```haskell
fibs 0 = 1
fibs 1 = 1
fibs n = fibs (n-1) + fibs (n-2)
```

### if-then-else

```haskell
if <condition> then <case 1> else <case 2>
```

just like JavaScript’s ternary if operator: `<condition> ? <case 1> : <case 3>`

```haskell
fibs n = if n == 0 then 1 else if n == 1 then 1 else fibs (n-1) + fibs (n-2)
```

### Guards

Can test `Bool` expressions (i.e. not just values matching as in pattern matching)

```haskell
fibs n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fibs (n-1) + fibs (n-2)
```

### case

```haskell
case <expression> of
  <pattern1> -> <result if pattern1 matches>
  <pattern2> -> <result if pattern2 matches>
  _ -> <result if no pattern above matches>
```

For example:

```haskell
fibs n = case n of
  0 -> 1
  1 -> 1
  _ -> fibs (n-1) + fibs (n-2)
```

</div>

## Glossary

*GHCi REPL*: The interactive Read-Eval-Print Loop for GHC, the Glasgow Haskell Compiler, allowing users to test Haskell programs and expressions interactively.

*Pattern Matching*: A mechanism in Haskell that checks a value against a pattern. It is used to simplify code by specifying different actions for different input patterns.

*Guards*: A feature in Haskell used to test boolean expressions. They provide a way to conditionally execute code based on the results of boolean expressions.

*Where Clauses*: A way to define local bindings in Haskell, allowing variables or functions to be used within a function body.

*Let Clauses*: A way to bind variables or functions within an expression in Haskell, allowing for more localised definitions.

*Hoogle*: A Haskell API search engine that allows users to search for functions by name or by type signature.

*Prelude*: The default library loaded in Haskell that includes basic functions and operators.

*Case Expressions*: A way to perform pattern matching in Haskell that allows for more complex conditional logic within expressions.

*Type Class*: A type system construct in Haskell that defines a set of functions that can be applied to different types, allowing for polymorphic functions.

*Unit*: A type with exactly one value, (), used to indicate the absence of meaningful return value, similar to void in other languages.
