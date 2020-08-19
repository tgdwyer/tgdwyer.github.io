---
layout: page
title: "Refined Haskell"
permalink: /haskell3/
---

In this chapter we see how the Haskell language features we introduced in previous chapters (from function application rules based on [Lambda Calculus](/lambdacalculus) to [Typeclasses](/haskell2#typeclasses)) lead to highly flexible and refactorable code and powerful abstractions.

## Learning Outcomes

- Understand how [eta-conversion](/haskell3#eta-conversion), [operator sectioning](/haskell3#operator-sectioning) and [compose](/haskell3#compose), together provide the ability to transform code to achieve a composable [point free](/haskell3#point-free-code) form and use this technique to refactor code.
- Understand that in Haskell the ability to map over container structures is generalised into the [Functor](/haskell3#functor) typeclass, such that any type that is an instance of Functor has the `fmap` or `(<$>)` operation.
- Understand that the [Applicative Typeclass](/haskell3#applicative) extends Functor such that containers of functions may be applied (using the `(<*>)` operator) to containers of values.
- Understand that [Foldable](/haskell3#foldable) generalises containers that may be folded (or reduced) into values
- Understand that [Traversable](/haskell3#traversable) generalises containers over which we can traverse applying a function with an Applicative effect
- Understand that [Monad](/haskell3#monad) extends Functor and Applicative to provide a bind `(>>=)` operation which allows us to sequence effectful operations such that their effects are flattened or joined into a single effect.

<div class="cheatsheet" markdown="1">

## Refactoring Cheatsheet

The following equivalences make many refactorings possible in Haskell:

### Eta Conversion
Exactly as per [Lambda Calculus](/lambdacalculus):
```haskell
 f x ≡ g x
 f   ≡ g
```

### Operator Sectioning
Remember haskell binary operators are just infix curried functions of two parameters and that putting brackets around them makes them prefix instead of infix.

```haskell
 x + y ≡ (+) x y
       ≡ ((+) x) y
```

### Compose
Has its own operator in haskell `(.)`, inspired by the mathematical function composition symbol `∘`:
```haskell
 (f ∘ g) (x) ≡ f (g(x)) -- math notation
 (f . g) x ≡ f (g x)    -- haskell
```
</div>
## Point Free Code

We have discussed point-free and tacit coding style earlier in these notes. In particular, eta-conversion works in Haskell the same as in lambda calculus and for curried JavaScript functions.  It is easy to do and usually declutters code of unnecessary arguments that help to distill their essence, e.g.:

```haskell
lessThanNum :: Num a => a -> [a] -> [a]
lessThanNum n aList = filter (<n) aList
```

The following is more concise, and once you are used to reading haskell type definitions, just as self evident:

```haskell
lessThanNum :: Num a => a -> [a] -> [a]
lessThanNum n = filter (<n)
```

But the above still has an argument (a point), `n`.  Can we go further?

It is possible to be more aggressive in refactoring code to achieve point-free style by using the compose operator `(.)`:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)
```

To see how to use `(.)` in `lessThanNum` we need to refactor it to look like the right-hand side of the definition above, i.e. `f (g x)`.  For lessThanNum, this takes a couple of steps, because the order we pass arguments to `(<)` matters.  Partially applying infix operators like `(<n)` is called operator sectioning.  Placing `n` after `<` means that it is being passed as the second argument to the operator, which is inconvenient for eta-conversion.  Observe that `(<n)` is equivalent to `(n>)`, so the following is equivalent to the definition above:

```haskell
lessThanNum n = filter (n>)
```

Now we can use the non-infix form of (>):
```haskell
lessThanNum n = filter ((>) n)
```

And we see from [our definition of compose](/haskell3#compose), that if we were to replace filter by `f`, `(>)` by `g`, and `n` by `x`, we would have exactly the definition of `(.)`.  Thus,

```haskell
lessThanNum n = (filter . (>)) n
```

And now we can apply [eta-conversion](/haskell3#eta-conversion):

```haskell
lessThanNum = filter . (>)
```

Between operator sectioning, the compose combinator `(.)`, and eta-conversion it is possible to write many functions in point-free form.  For example, the flip combinator:

```haskell
flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a
```

can also be useful in reversing the arguments of a function or operator in order to get them into a position such that they can be eta-reduced.
  
In code written by experienced haskellers it is very common to see functions reduced to point-free form.  Does it make it for more readable code?  To experienced haskellers, many times yes.  To novices, perhaps not.  When to do it is a matter of preference.  Experienced haskellers tend to prefer it, they will argue that it reduces functions like the example one above “to their essence”, removing the “unnecessary plumbing” of explicitly named variables.   Whether you like it or not, it is worth being familiar with the tricks above, because you will undoubtedly see them used in practice.  The other place where point free style is very useful is when you would otherwise need to use a lambda function.

Some more (and deeper) discussion is available on the Haskell Wiki.

### Exercises

* Refactor the following function to be point-free:

```haskell
f a b c = (a+b)*c
```

(This is clearly an extreme example but is a useful - and easily verified - practice of operator sectioning, composition and eta-conversion.)

## Functor

We’ve been mapping over lists and arrays many times, first in JavaScript:

```javascript
console> [1,2,3].map(x=>x+1)
[2,3,4]
```

Now in haskell:

```haskell
GHCi> map (\i->i+1) [1,2,3]
[2,3,4]
```

Or (eta-reduce the lambda to be point-free):

```haskell
GHCi> map (+1) [1,2,3]
[2,3,4]
```

Here’s the implementation of `map` for lists as it’s [defined in the GHC standard library](http://hackage.haskell.org/package/base-4.11.1.0/docs/src/GHC.Base.html#map):

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs 
```

It’s easy to generalise this pattern to any data structure that holds one or more values: mapping a function over a data structure creates a new data structure whose elements are the result of applying the function to the elements of the original data structure.

In Haskell this pattern is captured in a type class called `Functor`, which defines a function called `fmap`.

```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
```

Naturally, lists have an instance:

```haskell
Prelude> :i []
...
instance Functor [] -- Defined in `GHC.Base'
```

We can actually look up GHC’s implementation of `fmap` for lists and we see:

```haskell
instance Functor [] where
    fmap = map
```

And here’s the instance for `Maybe`:

```haskell
instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)
```

So we can `fmap` a function over a Maybe without having to unpackage it:
```haskell
GHCi> fmap (+1) (Just 6)
Just 7 
```

This is such a common operation that there is an operator alias for fmap: <$>
```haskell
GHCi> (+1) <$> (Just 6)
Just 7 
```

Which also works over lists:
```haskell
GHCi> (+1) <$> [1,2,3]
[2,3,4] 
```

Lists of Maybes frequently arise.  For example, the “mod” operation on integers (e.g. mod 3 2 == 1) will throw an error if you pass 0 as the divisor:
```haskell
> mod 3 0
*** Exception: divide by zero 
```

We might define a safe modulo function:
```haskell
safeMod :: Integral a => a-> a-> Maybe a
safeMod _ 0 = Nothing
safeMod numerator divisor = Just $ mod numerator divisor 
```

This makes it safe to apply safeMod to an arbitrary list of Integral values:
```haskell
> map (safeMod 3) [1,2,0,4]
[Just 0,Just 1,Nothing,Just 3] 
```

But how do we keep working with such a list of Maybes?  We can map an fmap over the list:

```haskell
GHCi> map ((+1) <$>) [Just 0,Just 1,Nothing,Just 3]
[Just 1,Just 2,Nothing,Just 4] 
```

Or equivalently:
```haskell
GHCi> ((+1) <$>) <$> [Just 0,Just 1,Nothing,Just 3]
[Just 1,Just 2,Nothing,Just 4] 
```

In addition to lists and Maybes, a number of other built-in types have instances of Functor:

```haskell
GHCi> :i Functor
instance Functor (Either a) -- Defined in `Data.Either'
instance Functor [] -- Defined in `GHC.Base'
instance Functor Maybe -- Defined in `GHC.Base'
instance Functor IO -- Defined in `GHC.Base'
instance Functor ((->) r) -- Defined in `GHC.Base'
instance Functor ((,) a) -- Defined in `GHC.Base' 
```

The definition for functions `(->)` might surprise:
```haskell
instance Functor ((->) r) where
    fmap = (.) 
```

So the composition of functions `f` and `g`: `f . g`, is equivalent to ‘mapping’ `f` over `g`, e.g.  `f <$> g`.  

```haskell
GHCi> f = (+1)
GHCi> g = (*2)
GHCi> (f.g) 3
7
GHCi> (f<$>g) 3
7
```

### Functor Laws
We can formalise the definition of Functor with two laws:
The law of ***identity*** 

∀ x: (id <$> x) ≡ x 

The law of ***composition***

∀ f, g, x: (f ∘ g <$> x) ≡ (f <$> (g <$> x)) 

Note that these laws are not enforced by the compiler when you create your own instances of `Functor`.  You’ll need to test them for yourself.  Following these laws guarantees that general code (e.g. algorithms) using `fmap` will also work for your own instances of `Functor`.

## Applicative

The applicative introduces a new operator `<*>` (pronounced “apply”), which lets us apply functions inside a computational context.

For example, a function inside a Maybe can be applied to a value in a Maybe.
```haskell
GHCi> (Just (+3)) <*> (Just 2)
Just 5 
```

Or a list of functions [(+1),(+2)] ), to things inside a similar context (e.g. a list [1,2,3]).
```haskell
> [(+1),(+2)] <*> [1,2,3]
[2,3,4,3,4,5] 
```

Note that lists definition of <*> produces the cartesian product of the two lists, that is, all the possible ways to apply the functions in the left list, to the values in the right list.  It is interesting to look at the source for the definition of Applicative for lists on Hackage:

instance Applicative [] where
```haskell
pure x    = [x]
fs <*> xs = [f x | f <- fs, x <- xs]  -- list comprehension 
```

The definition of <*> for lists uses a list comprehension.  List comprehensions are a short-hand way to generate lists, using notation similar to mathematical set builder notation.  The set builder notation here would be:  f(x) |  ffs x  xs.  In English it means: “the set (Haskell list) of all functions in fs applied to all values in xs”. 


A common use-case for Applicative is applying a binary (two-parameter) function over two Applicative values, e.g.:
```haskell
> (+) <$> Just 3 <*> Just 2
Just 5 
```

This is called “lifting” a function over Applicative.  Actually, it’s so common that Applicative also defines dedicated functions for lifting binary functions (in the GHC.Base module):

```haskell
> GHC.Base.liftA2 (+) (Just 3) (Just 2)
Just 5 
```

It’s also useful to lift binary data constructors over two Applicative values, e.g. for tuples:
```haskell
> (,) <$> Just 3 <*> Just 2
Just (3, 2) 
```

Or lifting a data constructor over lists:

```haskell
data Suit = Spade|Club|Diamond|Heart
 deriving (Eq,Ord,Enum,Bounded)

instance Show Suit where
 show Spade = "^"     -- ♠	 (closest I could come in ASCII was ^)
 show Club = "&"      -- ♣
 show Diamond = "O"   -- ♦
 show Heart = "V"     -- ♥

data Rank = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
 deriving (Eq,Ord,Enum,Show,Bounded)

data Card = Card Suit Rank
 deriving (Eq, Ord, Show) 
```

We can make one card using the Card constructor:
```haskell
GHCi> Card Spade Ace
Card ^ Ace 
```

Or, since both Suit and Rank derive Enum, we can enumerate the full lists of Suits and Ranks, and then lift the Card operator over both lists to create a whole deck:
```haskell
GHCi> Card <$> [Spade ..] <*> [Two ..]
[Card ^ Two,Card ^ Three,Card ^ Four,Card ^ Five,Card ^ Six,Card ^ Seven,Card ^ Eight,Card ^ Nine,Card ^ Ten,Card ^ Jack,Card ^ Queen,Card ^ King,Card ^ Ace,Card & Two,Card & Three,Card & Four,Card & Five,Card & Six,Card & Seven,Card & Eight,Card & Nine,Card & Ten,Card & Jack,Card & Queen,Card & King,Card & Ace,Card O Two,Card O Three,Card O Four,Card O Five,Card O Six,Card O Seven,Card O Eight,Card O Nine,Card O Ten,Card O Jack,Card O Queen,Card O King,Card O Ace,Card V Two,Card V Three,Card V Four,Card V Five,Card V Six,Card V Seven,Card V Eight,Card V Nine,Card V Ten,Card V Jack,Card V Queen,Card V King,Card V Ace] 
```


<div class="cheatsheet" markdown="1">
### Different Ways To Apply Functions Cheatsheet

```haskell
 g x         -- apply function g to argument x
 g $ x       -- apply function g to argument x
 g <$> f x   -- apply function g to argument x which is inside Functor f
 f g <*> f x -- apply function g in Applicative context f to argument x which is also inside f
```
</div>

Applicative is a “subclass” of Functor, meaning that an instance of Applicative can be ‘fmap’ed, but Applicatives also declare (at least) two additional functions, pure and `(<*>)` (pronounced ‘apply’ - but I like calling it [“TIE Fighter”](https://en.wikipedia.org/wiki/TIE_fighter)):

```haskell
GHCi> :i Applicative
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b 
...
```

As for `Functor`, many Base Haskell types are also `Applicative`, e.g. `[]`, `Maybe`, `IO` and `(->)`.

## Foldable
Recall the “reduce” function that is a member of JavaScript’s Array type, and which we implemented ourselves for linked and cons lists, was a way to generalise loops over enumerable types.
In Haskell, this is concept is once again generalised with a typeclass called Foldable - the class of things which can be “folded” over to produce a single value.  The obvious Foldable instance is list.  Although in JavaScript reduce always associates elements from left to right, haskell offers two functions foldl, which folds left to right) and foldr (which folds right to left):

```haskell
GHCi> :t foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b 

GHCi> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

Here’s how we right-fold over a list to sum its elements:
```haskell
GHCi> foldr (\i a -> i + a) 0 [5,8,3,1,7,6,2]
32 
```

Actually, this is a classic example where point-free coding style makes this expression very succinct: `foldr (+) 0 [5,8,3,1,7,6,2]`
...but the lambda above makes it clear which parameter is the accumulator and which is the list element.  Here’s a left fold with a picture of the fold:

![Left Fold](/haskell3/leftfold.png)

In the fold above, we provide the `(+)` function to tell `foldl`|`r` how to aggregate elements of the list.  There is also a typeclass for things that are “automatically aggregatable” or “concatenatable” called `Monoid` which declares a general concatenation function for Monoid called `mconcat`.  One instance of Monoid is `Sum`, since there is an instance of `Sum` for `Num`:
```haskell
GHCi> :i Monoid
…
instance Num a => Monoid (Sum a)
…
GHCi> import Data.Monoid
Data.Monoid> mconcat $ Sum <$> [5,8,3,1,7,6,2]
Sum {getSum = 32} 
```

So a sum is a data type with an accessor function getSum that we can use to get back the value:
```haskell
GHCi Data.Monoid> getSum $ mconcat $ Sum <$> [5,8,3,1,7,6,2]
32 
```

We make a data type aggregatable by instancing Monoid and providing definitions for the functions mappend and mempty.  For Sum these will be (+) and 0 respectively.
Lists are also themselves Monoidal, with mappend defined as an alias for list concatenation (++), and mempty as [].  Thus, we can:
```haskell
GHCi Data.Monoid> mconcat [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6] 
```

Which has a simple alias “concat” defined in the Prelude:
```haskell
GHCi> concat [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6]
```

## Traversable

The following map shows how all of these typeclasses are starting to come together to offer some real power:

![Traversable Typeclasses](/haskell3/traversabletypeclasses.png)

Remember our safe modulo function:
```haskell
safeMod :: Integral a => a-> a-> Maybe a
safeMod _ 0 = Nothing
safeMod numerator divisor = Just $ mod numerator divisor 
```

Which we could use to map over a list of numbers without throwing divide-by-zero exceptions:
```haskell
GHCi> map (safeMod 3) [1,2,0,2]
[Just 0,Just 1,Nothing,Just 1]
But what if 0s in the list really are indicative of disaster so that we should bail rather than proceeding?  The traverse function of the Traversable type-class gives us this capability:
GHCi> traverse (safeMod 3) [1,2,0,2]
Nothing 
```

Traverse applies a function with an Applicative return value (or Applicative effect) to the contents of a Traversable thing.
```haskell
GHCi> :t traverse
traverse
  :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b) 
```

What are some other functions with Applicative effects?  Lots! E.g.:

- Any constructor of a data type that instances Applicative: e.g. `Just :: a -> Maybe a`
- Anything that creates a list: `(take 5 $ repeat 1) :: Num a => [a]`
- IO is Applicative, so a function like `print :: Show a => a -> IO ()`
- etc...

The `print` function converts values to strings (using show if available from an instance of `Show`) and sends them to standard-out.  The `print` function wraps this effect (there is an effect on the state of the console) in an `IO` computational context:

```haskell
GHCi> :t print
print :: Show a => a -> IO ()
The () is like void in TypeScript - it’s a type with exactly one value (), and hence is called “Unit”.  There is no return value from print, only the IO effect, and hence the return type is IO (). IO is also an instance of Applicative.  This means we can use traverse to print out the contents of a list:
GHCi> traverse print [1,2,3]
1
2
3
[(),(),()] 
```
Where the return value is a list of the effects of each application of print, inside the `IO` Applicative.
```haskell
GHCi> :t traverse print [1,2,3]
traverse print [1,2,3] :: IO [()] 
```

There is no easy way to get rid of this IO return type - which protects you from creating `IO` effects unintentionally.

A related function defined in `Traversable` is `sequenceA` allows us to convert directly from Traversables of Applicatives, to Applicatives of Traversables:
```haskell
> :t sequenceA
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
GHCi> sequenceA [Just 0,Just 1,Nothing,Just 1]
Nothing
Or on a “clean” list of Just values:
GHCi> sequenceA [Just 0,Just 1,Just 1]
Just [0,1,1] 
```

## Monad
As always, we can interrogate ghci to get a basic synopsis of the Monad typeclass:

```haskell
> :i Monad
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
...
  {-# MINIMAL (>>=) #-}
        -- Defined in `GHC.Base'
instance Monad (Either e) -- Defined in `Data.Either'
instance [safe] Monad m => Monad (ReaderT r m)
  -- Defined in `transformers-0.5.2.0:Control.Monad.Trans.Reader'
instance Monad [] -- Defined in `GHC.Base'
instance Monad Maybe -- Defined in `GHC.Base'
instance Monad IO -- Defined in `GHC.Base'
instance Monad ((->) r) -- Defined in `GHC.Base'
instance Monoid a => Monad ((,) a) -- Defined in `GHC.Base' 
```

Things to notice:

* `Monad` is a subclass of `Applicative` (and therefore also a `Functor`)
* `return` = `pure`, from `Applicative`.  `return` exists for historical reasons and you can safely use only `pure` (PureScript has only `pure`).
* the operator `(>>=)` (pronounced “bind”) is the minimal definition (the one function you must create--in addition to the functions also required for `Functor` and `Applicative`--to make a new `Monad` instance).
* `>>` is special case of bind (described below)
* lots of built-in types are already monads

There also exists a flipped version of bind:
```haskell
(=<<) = flip (>>=) 
```

Its type has a nice correspondence to the other operators we have already seen for function application in various contexts:

```haskell
(=<<) :: Monad m       => (a -> m b) -> m a -> m b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(<$>) :: Functor f     =>   (a -> b) -> f a -> f b
($)   ::                    (a -> b) -> a   -> b 
```

### IO

The haskell type which captures Input/Output effects is called `IO`.  As we demonstrated with the `traverse` function, it is possible to perform `IO` actions using `fmap` `(<$>)` and applicative `(<*>)`, (for example printing to the console), the challenge is taking values out of an `IO` context and using them to create further `IO` effects.

Here are some simple `IO` “actions”:
```haskell
sayHi :: IO ()
sayHi = putStrLn "Hi, what's your name?"
readName :: IO String
readName = getLine
greet :: String -> IO ()
greet name = putStrLn ("Nice to meet you, " ++ name ++ ".") 
```

The following typechecks:
```haskell
main = greet <$> getLine 
```

When you run it from either GHCi or an executable compiled with ghc, it will pause and wait for input, but you will not see the subsequent greeting.
This is because the type of the expression is:

```haskell
GHCi> :t greet <$> getLine
greet <$> getLine :: IO (IO ()) 
```

The `IO` action we want (`greet`) is nested inside another `IO` action.  When it is run, only the outer `IO` action is actually executed. The inner `IO` computation (action) is not actually touched.
To see an output we somehow need to flatten the `IO (IO ())` into just a single level: `IO ()`.
`(>>=)` gives us this ability:

```haskell
GHCi> :t getLine >>= greet
getLine >>= greet :: IO ()

GHCi> getLine >>= greet
```

>Tim  
>Nice to meet you Tim!

The special case of bind `(>>)` allows us to chain actions without passing through a value:

```haskell
GHCi> :t (>>)
(>>) :: Monad m => m a -> m b -> m b

GHCi> sayHi >> getLine >>= greet
```
> Hi, what's your name?  
> Tim  
> Nice to meet you Tim!


### Do notation

Haskell gives us syntactic sugar for bind in the form of “do blocks”:

```haskell
main :: IO ()
main = do
   sayHi
   name <- readName
   greet name 
```

Which is entirely equivalent to the above code, or more explicitly:

```haskell
main =
   sayHi >>
   readName >>=
   \name -> greet name 
```

Note that although <- looks like assignment to a variable named “name”, it actually expands to a parameter name for a lambda expression following the bind.  Thus, the way I read the following do expression is “take the value (a string in this case) out of the Monad context resulting from the function (readName) and assign to the symbol (name)”:
```haskell
do
  name <- readName
  greet name 
```

You can also mix in actual variable assignments using let:

```haskell
do
  name <- readName
  let greeting = "Hello " ++ name
  putStrLn greeting 
```

### Join
A function called “join” from `Control.Monad` also distills the essence of `Monad` nicely.  Its type and definition in terms of bind is:
```haskell
join :: Monad m => m (m a) -> m a
join = (>>=id)
```

We can apply join to “flatten” the nested IO contexts from the earlier fmap example:
```haskell
GHCi>:t join $ greet <$> getLine :: IO () 
```

Which will now execute as expected:
```haskell
GHCi> join $ greet <$> getLine
```
>Tim  
>Nice to meet you Tim! 

