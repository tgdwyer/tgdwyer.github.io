---
layout: page
title: "Foldable and Traversible"
permalink: /haskell4/
---

In this chapter we will meet some more typeclasses that abstract common coding patterns for dealing with data.

## Learning Outcomes

- Understand that the "reduce" function we met for arrays and other data structures in JavaScript is referred to as ["folding"](/haskell4/#folds) in Haskell and there are two variants `foldl` and `foldr` for left and right folds respectively.
- Understand that the [Monoid](/haskell4#monoid)
- Understand that [Foldable](/haskell4#foldable) generalises containers that may be folded (or reduced) into values
- Understand that [Traversable](/haskell4#traversable) generalises containers over which we can traverse applying a function with an Applicative effect
- Understand that [Monad](/haskell4#monad) extends Functor and Applicative to provide a bind `(>>=)` operation which allows us to sequence effectful operations such that their effects are flattened or joined into a single effect.

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

:i Monoid
type Monoid :: * -> Constraint
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

So the bind function `(>>=)` (and equally its flipped version `(=<<)`) gives us another way to map functions over contexts, but why do we need another way?

As an example we'll consider computation using the `Maybe` type, which we said is useful for partial functions, that is functions which are not sensibly defined over all of their inputs.  A good example of such a function is the [quadratic formula](https://en.wikipedia.org/wiki/Quadratic_formula), which for quadratic functions of the form:

![quadratic function](/haskell4/quadratic.png)

Determines two roots as follows:

![quadratic roots](/haskell4/quadroots.png)

This may fail in two ways.  First, if *a* is 0, second if the expression that squareroot is applied to is negative (and we insist on only real-valued solutions).  Therefore, let's define a little library of math functions which encapsulate the possibility of failure in a `Maybe`:

```haskell
safeDiv :: Float -> Float -> Maybe Float
safeDiv _ 0 = Nothing
safeDiv numerator denominator = Just $ numerator / denominator

safeSquareroot :: Float -> Maybe Float
safeSquareroot x = if x < 0 then Nothing else Just $ sqrt x
```

Great!  Now we can use `case` and pattern matching to make a safe solver of quadratic equations:

```haskell
safeSolve :: Float -> Float -> Float -> Maybe (Float, Float)
safeSolve a b c =
    case safeSquareroot $ b*b - 4 * a * c of
        Just s ->
            let
            x1 = safeDiv (-b + s) (2*a)
            x2 = safeDiv (-b - s) (2*a)
            in case (x1,x2) of
                (Just x1', Just x2') -> Just (x1',x2')
                _ -> Nothing
        Nothing -> Nothing

> safeSolve 1 3 2
Just (-1.0,-2.0)
> safeSolve 1 1 2
Nothing
```

Actually, not so great, we are having to unpack Maybes multiple times, leading to nested `case`s.  This is just two levels of nesting, what happens if we need to work in additional computations that can fail?

The general problem is that we need to chain multiple functions of the form `Float -> Maybe Float`.  Let's look again at the type of bind:

```haskell
> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

The first argument it expects is a value in a context `m a`.  What if that we apply it to a `Maybe Float`?

```haskell
> x = 1::Float
> :t (Just x>>=)
(Just x>>=) :: (Float -> Maybe b) -> Maybe b
```

So GHCi is telling us that the next argument has to be a function that takes a `Float` as input, and gives back anything in a `Maybe`.  Our `safeSquareroot` definitely fits this description, as does `safeDiv` partially applied to a Float.  So, here's a `safeSolve` which uses `(>>=)` to remove the need for `case`s:

```haskell
safeSolve :: Float -> Float -> Float -> Maybe (Float, Float)
safeSolve a b c =
    safeSquareroot (b*b - 4 * a * c) >>= \s ->
    safeDiv (-b + s) (2*a) >>= \x1 ->
    safeDiv (-b - s) (2*a) >>= \x2 ->
    pure (x1,x2)

> safeSolve 1 3 2
Just (-1.0,-2.0)
> safeSolve 1 1 2
Nothing
```

How is a `Nothing` result from either of our `safe` functions handled?  Well, the [Maybe instance of Monad](https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.Base.html#line-1005) defines bind like so:

```haskell
instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing
```

So that's one instance of `Monad`, let's look at some more...

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

Note that although `<-` looks like assignment to a variable `name`, it actually expands to a parameter name for a lambda expression following the bind.  Thus, the way I read the line with the `<-` in the following do expression:

```haskell
do
  name <- readName
  greet name 
```

is:

*Take the value* (a `String` in this case) 
*out of the Monad context resulting from the expression on the right-hand side of the `<-`* (i.e. `readName`) *and assign it to the symbol on the left-hand side* (i.e. `name`) *which remains in scope until the end of the `do` block:*

You can also mix in variable assignments from pure expressions using let:

```haskell
do
  name <- readName
  let greeting = "Hello " ++ name
  putStrLn greeting
```

### Join

A function called “`join`” from `Control.Monad` also distills the essence of `Monad` nicely.  Its type and definition in terms of bind is:

```haskell
join :: Monad m => m (m a) -> m a
join = (>>=id)
```

We can apply join to “flatten” the nested `IO` contexts from the earlier `fmap` example:

```haskell
GHCi>:t join $ greet <$> getLine :: IO ()
```

Which will now execute as expected:

```haskell
GHCi> join $ greet <$> getLine
```

>Tim  
>Nice to meet you Tim!