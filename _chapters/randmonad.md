---
layout: chapter
title: "Foldable and Traversable"
---

# Rand Monad

```haskell
newtype Rand a = Rand { next :: Seed -> (Seed, a) }
```

`Rand` is a newtype wrapper around a function `Seed -> (Seed, a)`.
It represents a computation that, given a starting Seed, produces:

1. A new updated `Seed`.
2. A value of type `a`.

![Rand Monad](/assets/images/chapterImages/randmonad/randMonad.png)

## Functor

Definition of `Functor`. The `Functor` instance for `Rand` allows you to map a function over the result of a random computation.

```haskell
instance Functor Rand where
  fmap :: (a -> b) -> Rand a -> Rand b
  fmap f (Rand g) = Rand h
    where
      -- The function inside rand
      -- Apply f to the `value` a
      h seed = (newSeed, f a)
        where
          (newSeed, a) = g seed
```

fmap constructs a new Rand value, Rand h, by:

1. Taking a function `f` and a random computation `Rand g`.
2. Defining a new function `h` that, given an initial Seed, runs `g` to get `(newSeed, a)`.
3. Returning `(newSeed, f a)`, where `f a` is the transformed value.

After applying fmap f, we have a new random computation that takes the same Seed as input and produces a transformed value (f a), while maintaining the same mechanics of randomness (i.e., correctly passing and updating the Seed state).

We can also be a bit more succinct, by making use of `fmap` instances

```haskell
fmap :: (a -> b) -> Rand a -> Rand b
fmap f r = Rand $ (f <$>)<$> next r
```

## Applicative

```haskell
pure :: a -> Rand a
pure x = Rand (,x) -- Return the input seed and the value


(<*>) :: Rand (a -> b) -> Rand a -> Rand b
left <*> right = Rand h
where
    h s = (s'', f v) -- Need to return a function of type (Seed -> (Seed, Value))
    where
        (s', f) = next left s   -- Get the next seed and function from the left Rand
        (s'', v) = next right s' -- Get the next seed and value from the right Rand
```

`<*>` constructs a new Rand value Rand h by:

1. Extracting a function `f` from the left computation using the initial seed.
2. Using the new seed to extract a value `v` from the right computation.
3. Returning a new seed and the result of applying `f` to `v`.
4. This allows us to apply a random function to a random value in a sequence while maintaining proper state management of the Seed.

## Monad

```haskell
instance Monad Rand where
  (>>=) :: Rand a -> (a -> Rand b) -> Rand b
  r >>= f = Rand $ \s -> 
    let (s1, val) = next r s
    in next (f val) s1
```

`r >>= f` creates a new Rand computation by:

1. Running the first computation `r` with the initial seed `s`.
2. Extracting the value `val` and the new seed `s1`.
3. Using `val` to determine the next random computation `f val`.
4. Running `f val` with the updated seed `s1` to produce the final result.

![Bind](/assets/images/chapterImages/randmonad/bind.png)

### `Get` and `Put`

`put` is used to set the internal state (the `Seed`) of the `Rand` monad. There is no value yet, hence we use the unit (`()`)

`put` allows us to **modify** the internal state (Seed) of a random computation.

```haskell
put :: Seed -> Rand ()
put newSeed = Rand $ \_ -> (newSeed, ())
```

`get` is used to retrieve the current state (the `Seed`) from the `Rand` monad.
It **does not modify** the state but instead returns the current seed as the result. This is achieved by putting the current seed in to the value part of the tuple.

Since, when we apply transformation on the tuple, we apply the transformation according to the value!

```haskell
get :: Rand Seed
get = Rand $ \s -> (s, s)
```

Using `get` and the monad instance, we can make a function to increase the seed by one.

```haskell
incrementSeed' :: Rand Seed
incrementSeed' = get >>= \s -> pure (s + 1) 
```

```haskell
incrementSeed :: Rand Seed
incrementSeed = do
  seed <- get  -- This gets the current seed
  return (seed + 1)  -- Increment the seed and put it in the 'state', we can do anything with the seed!
```

```bash
>>> next incrementSeed' 123
(123, 124)
```

## Modifying a seed

We want to modify the seed, assuming there is no value. This will simply apply a function `f` to the current seed.

```haskell
modify :: (Seed -> Seed) -> Rand ()
modify f = Rand $ \s -> (f s, ())
```

We can also write this using our `get` and `put`

```haskell
modify :: (Seed -> Seed) -> Rand ()
modify f = get >>= \s -> put (f s)
```

This function:

1. First retrieves the current seed (`get`).
2. Applies the function `f` to modify the seed.
3. Updates the internal state with `put`.

This computation returns () as its result, indicating that its purpose is to update the state, not to produce a value.

We can now write our `incrementSeed` in terms of modify

```haskell
incrementSeed :: Rand Seed
incrementSeed = do
  modify (+1)   -- Use `modify` to increment the seed by 1
  get           -- Return the updated seed
```

```haskell
nextRand :: Seed -> Seed
nextRand prevSeed = (a*prevSeed + c) `mod` m
  where -- Parameters for linear congruential RNG.
    a = 1664525
    c = 1013904223
    m = 2^32

-- | Generate a number between `l` and `u`.
genRand :: Int -> Int -> Seed -> Int
genRand l u seed = seed `mod` (u-l+1) + l
```

```haskell
rollDie :: Rand Int
rollDie = do
  modify nextRand -- update the current seed
  s <- get -- get retrieves the updated seed value s from the Rand monad's state.
  pure (genRand 1 6 s) -- computes a random number and puts back in the context
```

We can also write this using bind notation, where we `modify nextRand` to update the seed. We then use `>>` to ignore the result (i.e., the `()`). We use get to put the seed as the value, which is then binded on to `s` and used to generate a random number. We then use pure to update the value, the seed updating is handled by our bind!

```haskell
rollDie :: Rand Int
rollDie = modify nextRand >> get >>= \s -> pure (genRand 1 6 s)
```
