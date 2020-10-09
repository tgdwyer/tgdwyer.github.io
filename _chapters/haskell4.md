---
layout: page
title: "Foldable and Traversible"
permalink: /haskell4/
---

In this chapter we will meet some more typeclasses that abstract common coding patterns for dealing with data.

## Learning Outcomes

- Understand that the "reduce" function we met for arrays and other data structures in JavaScript is referred to as ["folding"](/haskell4/#folds) in Haskell and there are two variants `foldl` and `foldr` for left and right folds respectively.
- Understand that the [Monoid](/haskell4#monoid) typeclass for things that have a predefined rule for aggregation (concatenation), making containers of `Monoid` values trivial to `fold`
- Understand that [Foldable](/haskell4#foldable) generalises containers that may be folded (or reduced) into values
- Understand that [Traversable](/haskell4#traversable) generalises containers over which we can traverse applying a function with an Applicative effect

## Folds

Recall the “`reduce`” function that is a member of JavaScript’s `Array` type, and which we implemented ourselves for linked and cons lists, was a way to generalise loops over enumerable types.
In Haskell, this concept is once again generalised with a typeclass called `Foldable` - the class of things which can be “folded” over to produce a single value.  
We will come back to [the `Foldable` typeclass](#foldable), but first let's look at the obvious `Foldable` instance.  
Although in JavaScript `reduce` always associates elements from left to right, Haskell's `Foldable` typeclass offers both `foldl` (which folds left-to-right) and `foldr` (which folds right-to-left):

```haskell
GHCi> :t foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

GHCi> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

Here’s how we right-fold over a list to sum its elements:

![Left Fold](/haskell4/rightfold.png)

While the lambda above makes it explicit which parameter is the accumulator and which is the list element, this is a classic example where point-free coding style makes this expression very succinct:

```haskell
GHCi> foldr (+) 0 [5,8,3,1,7,6,2]
```

> 32

Here’s a left fold with a picture of the fold:

![Left Fold](/haskell4/leftfold.png)

Note that since the `(+)` operator is commutative (`a+b=b+a`), it `foldr` and `foldl` return the same result.  For functions that are not commutative, however, this is not necessarily the case.

---------

### Exercise

* predict what the results of a left- and right-fold will be for `(-)` folded over `[1,2,3,4]` with initial value `0`.
* what is the result of `foldr (:) []` applied to any list?
* implement map using `foldr`

---------

## Monoid

In the example fold above, we provide the `(+)` function to tell `foldl` how to aggregate elements of the list.  There is also a typeclass for things that are “automatically aggregatable” or “concatenatable” called `Monoid` which declares a general function for `mappend` combining two `Monoid`s into one, a `mempty` value such that any Monoid `mappend`ed with `mempty` is itself, and a concatenation function for lists of `Monoid` called `mconcat`.  

```haskell
GHCi> :i Monoid
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty #-}
...
```

In the `Data.Monoid` library there are some interesting instances of Monoid. For example `Sum` is an instance of Monoid which wraps a `Num`, such that lists of `Sum` can be `mconcat`ed:

```haskell
GHCi> import Data.Monoid
Data.Monoid> mconcat $ Sum <$> [5,8,3,1,7,6,2]
```

> Sum {getSum = 32}

So a sum is a data type with an accessor function getSum that we can use to get back the value:

```haskell
GHCi Data.Monoid> getSum $ mconcat $ Sum <$> [5,8,3,1,7,6,2]
```

> 32

We make a data type aggregatable by instancing `Monoid` and providing definitions for the functions `mappend` and `mempty`.  For `Sum` these will be `(+)` and `0` respectively.
Lists are also themselves Monoidal, with `mappend` defined as an alias for list concatenation `(++)`, and mempty as `[]`.  Thus, we can:

```haskell
GHCi Data.Monoid> mconcat [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6]
```

Which has a simple alias `concat` defined in the Prelude:

```haskell
GHCi> concat [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6]
```

There is also an operator for `mappend` called `(<>)`, such the following are equivalent:

```haskell
Data.Monoid> mappend (Sum 1) (Sum 2)
Sum {getSum = 3}

Data.Monoid> (Sum 1) <> (Sum 2)
Sum {getSum = 3}
```

And for lists (and `String`) we have:

```haskell
> mappend [1,2] [3,4]
[1,2,3,4]

> [1,2] <> [3,4]
[1,2,3,4]

> [1,2] ++ [3,4]
[1,2,3,4]
```

## Foldable

So now we've already been introduced to `foldl` and `foldr` for lists, and we've also seen the `Monoid` typeclass, let's take a look at the general class of things that are `Foldable`.
As always, your best friend for exploring a new typeclass in Haskell is GHCi's `:i` command:

```haskell
Prelude> :i Foldable
type Foldable :: (* -> *) -> Constraint     
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b -- as described previously, but notice foldr and foldl
  foldl :: (b -> a -> b) -> b -> t a -> b -- are for any Foldable t, not only lists
  length :: t a -> Int -- number of items stored in the Foldable
  null :: t a -> Bool -- True if empty
  elem :: Eq a => a -> t a -> Bool -- True if the a is an element of the t of a
  maximum :: Ord a => t a -> a -- biggest element in the Foldable
  minimum :: Ord a => t a -> a -- smallest element
  sum :: Num a => t a -> a -- compute the sum of a Foldable of Num
  product :: Num a => t a -> a -- compute the product of a Foldable of Num
  Data.Foldable.fold :: Monoid m => t m -> m -- if the elements of t are Monoids then we don't need an operator to aggregate them
  foldMap :: Monoid m => (a -> m) -> t a -> m -- uses the specified function to convert elements to Monoid and then folds them
  Data.Foldable.toList :: t a -> [a] -- convert any Foldable things to a list
  {-# MINIMAL foldMap | foldr #-}
        -- Defined in `Data.Foldable'
instance Foldable [] -- Defined in `Data.Foldable'
instance Foldable Maybe -- Defined in `Data.Foldable'
instance Foldable (Either a) -- Defined in `Data.Foldable'
instance Foldable ((,) a) -- Defined in `Data.Foldable'
```

Note that I've reordered the list of functions to the order we want to discuss them, removed a few things we're not interested in at the moment and the comments are mine.
However, once you get used to reading types the type info for this class is pretty self explanatory.

## Traversable

`Traversable` extends both `Foldable` and `Functor`, in a typeclass for things that we can `traverse` a function with an `Applicative` effect over, e.g. here's a sneak peak of what this lets us do:

```haskell
ghci> traverse putStrLn ["tim","was","here"]
tim
was
here
[(),(),()]
```

The first three lines are the strings printed to the terminal (the side effect).  The result reported by GHCi is a list `[(),(),()]` as discussed below.

Here, as usual, is what GHCi `:i` tells us about the Traversable type class:

```haskell
ghci> :i Traversable
type Traversable :: (* -> *) -> Constraint
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  ... -- some other functions
  {-# MINIMAL traverse | sequenceA #-}
        -- Defined in `Data.Traversable'
instance Traversable [] -- Defined in `Data.Traversable'
instance Traversable Maybe -- Defined in `Data.Traversable'
instance Traversable (Either a) -- Defined in `Data.Traversable'
instance Traversable ((,) a) -- Defined in `Data.Traversable'
```

The following map shows how all of these typeclasses are starting to come together to offer some real power:

![Traversable Typeclasses](/haskell4/traversabletypeclasses.png)

So what does the traverse function do?  By way of example, remember our safe modulo function this we used to experiment with [Functor](/haskell3/#functor):
```haskell
safeMod :: Integral a => a-> a-> Maybe a
safeMod _ 0 = Nothing
safeMod numerator divisor = Just $ mod numerator divisor 
```

It lets us map over a list of numbers without throwing divide-by-zero exceptions:

```haskell
GHCi> map (safeMod 3) [1,2,0,2]
[Just 0,Just 1,Nothing,Just 1]
```

But what if `0`s in the list really are indicative of disaster so that we should bail rather than proceeding?  The `traverse` function of the `Traversable` type-class gives us this capability:

```haskell
GHCi> traverse (safeMod 3) [1,2,0,2]
Nothing 
```

Traverse applies a function with a result in an `Applicative` context (i.e. an Applicative effect) to the contents of a `Traversable` thing.

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
```

The `()` is like `void` in TypeScript - it’s a type with exactly one value `()`, and hence is called “Unit”.  There is no return value from `print`, only the `IO` effect, and hence the return type is `()`.  `IO` is also an instance of `Applicative`.  This means we can use `traverse` to print out the contents of a list:

```haskell
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

There is no easy way to get rid of this `IO` return type - which protects you from creating `IO` effects unintentionally.

A related function defined in `Traversable` is `sequenceA` allows us to convert directly from Traversables of Applicatives, to Applicatives of Traversables:

```haskell
> :t sequenceA
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
GHCi> sequenceA [Just 0,Just 1,Just 1]
Just [0,1,1]
GHCi> sequenceA [Just 0,Just 1,Nothing,Just 1]
Nothing
```

The default `sequenceA` [is defined](https://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.Traversable.html#sequenceA) very simply in terms of `traverse` (recall `id` is just `\x->x`):

```haskell
sequenceA = traverse id
```

