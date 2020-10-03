---
layout: page
title: "Monad"
permalink: /monad/
---
## Learning Outcomes

- Understand that Monad extends [Functor and Applicative](/haskell3/) to provide a bind `(>>=)` operation which allows us to sequence effectful operations such that their effects are flattened or joined into a single effect.
- Understand the operation of bind in the `Maybe`, `IO` and List instances of Monad.

## Introduction

As with Functor and Applicative the name Monad comes from Category Theory.  Although the names sound mathematical they are abstractions of relatively simple concepts.  A Functor allowed unary functions to be applied (mapped/`fmap`ed) over a context.  Applicative allowed us to apply a function in a context to values in a context.  So too, Monad has a characteristic function called `bind`, which allows us to perform another type of function application over values in a context.

The special thing about bind is that it allows us to chain functions which have an effect without creating additional layers of nesting inside effect contexts.  People often try to describe Monads in metaphors, which are not always helpful.  The essence of Monad really is bind and there is no getting around looking at it's type signature and seeing what it does in different instances, which we will get to shortly.  However, one analogy that resonated for me was the idea of bind as a ["programmable semicolon"](http://book.realworldhaskell.org/read/monads.html).  That is, imagine a language like JavaScript which uses semicolons (`;`) as a statement separator:

```javascript
// some javascript you can try in a browser console:
const x = prompt("Name?"); console.log("Hello "+x)
```

As we will see shortly, the Haskell bind operator `>>=` can also be used to sequence expressions with an IO effect:

```haskell
getLine >>= \x -> putStrLn("hello "++x)
```

However, it not only separates the two expressions, it is safely handling the `IO` type within which all code with IO side-effects in Haskell must operate.  But as well as allowing us to chain operations, bind is defined to do different and useful things for different Monad instances, as we shall see.

## The Monad Typeclass

As always, we can interrogate GHCi to get a basic synopsis of the Monad typeclass:

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
* `return` = `pure`, from [`Applicative`](/haskell3/#applicative). The `return` function exists for historical reasons and you can safely use only `pure` (PureScript has only `pure`).
* the operator `(>>=)` (pronounced “bind”) is the minimal definition (the one function you must create--in addition to the functions also required for `Functor` and `Applicative`--to make a new `Monad` instance).
* `>>` is special case of bind (described below)
* lots of built-in types are already monads

There also exists a flipped version of bind:

```haskell
(=<<) = flip (>>=) 
```

The type of the flipped bind `(=<<)` has a nice correspondence to the other operators we have already seen for function application in various contexts:

```haskell
(=<<) :: Monad m       => (a -> m b) -> m a -> m b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(<$>) :: Functor f     =>   (a -> b) -> f a -> f b
($)   ::                    (a -> b) -> a   -> b
```

So the bind function `(>>=)` (and equally its flipped version `(=<<)`) gives us another way to map functions over contexts, but why do we need another way?

As an example we'll consider computation using the `Maybe` type, which we said is useful for [partial functions](/haskell2/#maybe), that is functions which are not sensibly defined over all of their inputs.  A more complex example of such a function than we have seen before is the [quadratic formula](https://en.wikipedia.org/wiki/Quadratic_formula), which for quadratic functions of the form:

<image src="/haskell4/quadratic.png" width="25%"></image>

determines two roots as follows:

<image src="/haskell4/quadroots.png" width="80%"></image>

This may fail in two ways.  First, if *a* is 0, second if the expression that squareroot is applied to is negative (and we insist on only real-valued solutions).  Therefore, let's define a little library of math functions which encapsulate the possibility of failure in a `Maybe`:

```haskell
safeDiv :: Float -> Float -> Maybe Float
safeDiv _ 0 = Nothing
safeDiv numerator denominator = Just $ numerator / denominator

safeSqrt :: Float -> Maybe Float
safeSqrt x
  | x < 0 = Nothing
  | otherwise = Just $ sqrt x -- the built-in squareroot function
```

Great!  Now we can use `case` and pattern matching to make a safe solver of quadratic equations:

```haskell
safeSolve :: Float -> Float -> Float -> Maybe (Float, Float)
safeSolve a b c =
    case safeSqrt $ b*b - 4 * a * c of
        Just s ->
            let x1 = safeDiv (-b + s) (2*a)
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

So GHCi is telling us that the next argument has to be a function that takes a `Float` as input, and gives back anything in a `Maybe`.  Our `safeSqrt` definitely fits this description, as does `safeDiv` partially applied to a `Float`.  So, here's a `safeSolve` which uses `(>>=)` to remove the need for `case`s:

```haskell
safeSolve :: Float -> Float -> Float -> Maybe (Float, Float)
safeSolve a b c =
    safeSqrt (b*b - 4 * a * c) >>= \s ->
    safeDiv (-b + s) (2*a) >>= \x1 ->
    safeDiv (-b - s) (2*a) >>= \x2 ->
    pure (x1,x2)

> safeSolve 1 3 2
Just (-1.0,-2.0)
> safeSolve 1 1 2
Nothing
```

Note that Haskell has a special notation for such multi-line use of bind, called "`do` notation".  The above code, in a `do` block looks like:

```haskell
safeSolve a b c = do
    s <- safeSqrt (b*b - 4 * a * c)
    x1 <- safeDiv (-b + s) (2*a)
    x2 <- safeDiv (-b - s) (2*a)
    pure (x1,x2)
```

So inside a `do`-block `y<-x` is completely equivalent to `x >>= \y -> ...`, where in both cases the variable `y` is in scope for the rest of the expression.  We'll see more [explanation and examples of `do` notation below](/monad/#do-notation).

How is a `Nothing` result from either of our `safe` functions handled?  Well, the [Maybe instance of Monad](https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.Base.html#line-1005) defines bind like so:

```haskell
instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing
```

Meaning, that anything on the right-hand side of a `Nothing>>=` will be left unevaluated and `Nothing` returned.

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

The `IO` action we want (`greet`) is nested inside another `IO` action.  When it is run, only the outer `IO` action is actually executed. The inner `IO` computation (action) is not evaluated.
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

## List

As with the [Applicative list instance](/haskell3/#applicative), the list implementation of bind [is defined](https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.Base.html#Monad) with comprehension syntax:

```haskell
instance Monad []  where
    xs >>= f = [y | x <- xs, y <- f x]
```

Where `xs` is a list and `f` is a function which returns a list.  `f` is applied to each element of `xs` and the result concatenated.  Actually, list comprehensions are just syntactic sugar for the list monad `do` notation, for example, `[(i,j)|i<-['a'..'d'],j<-[1..4]]` is equivalent to:

```haskell
do
  i <- ['a'..'d']
  j <- [1..4]
  pure (i,j)
```

> [('a',1),('b',1),('c',1),('d',1),('a',2),('b',2),('c',2),('d',2),('a',3),('b',3),('c',3),('d',3),('a',4),('b',4),('c',4),('d',4)]

Which is itself syntactic sugar for:

```haskell
['a'..'d'] >>= \i -> [1..4] >>= \j -> pure (i,j)
```

List comprehensions can also include conditional expressions which must evaluate to true for terms to be included in the list result.  For example, we can limit the above comprehension to only pairs with even `j`:

```haskell
[(i,j) | i<-['a'..'d'], j<-[1..4], j `mod` 2 == 0]
```

This comprehension syntax desugars to a `do`-block using the `guard` function from `Control.Monad` like so:

```haskell
import Control.Monad (guard)

do
  i <- ['a'..'d']
  j <- [1..4]
  guard $ j `mod` 2 == 0
  pure (i,j)
```

> [('a',2),('c',2),('e',2),('g',2),('a',4),('c',4),('e',4),('g',4)]
