# Eithers

## Learning Outcomes

- Understand how the `Either` type handles values with two possibilities, typically used for error handling and success cases.
- Apply the `Functor`, `Applicative`, and `Monad` type classes to the `Either` type, learning how to implement instances for each.
- Recognize the power of monadic `do` blocks in simplifying code and handling complex workflows.

## Introduction to Eithers

In Haskell, the Either type is used to represent values with two possibilities: a value of type Either a b is either Left a or Right b. By convention, Left is used to hold an error or exceptional value, while Right is used to hold a correct or expected value. This is particularly useful for error handling.

```haskell
data Either a b = Left a | Right b
```

In Haskell's `Either` type, convention ([and the official documentation](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Either.html)) says errors go on the `Left` and successes on the `Right`. Why? Because if it is not right (correct) it must be left. This can be considered another example bias against the left-handed people around the world, but alas, it is a [cruel world](https://www.youtube.com/watch?v=epvlvDzKfv8).

The Left/Right convention is also more general then a Success/Error naming, as Left does not always need to be an error, but it is the most common usage.

## Usage of Either

We can use `Either` to help us with error catching, similar to a `Maybe` type. However, since the error case, has a value, rather than `Nothing`, allowing to store an error message to give information to the programmer/user.

```haskell
divide :: Double -> Double -> Either String Double
divide _ 0 = Left "Division by zero error"
divide x y = Right (x / y)
```

Similar to Maybes, we can also use pattern matching against `Either`'s in a function.

```haskell
handleResult :: Either String Double -> String
handleResult (Left err) = "Error: " ++ err
handleResult (Right val) = "Success: " ++ show val
```

## The Kind of Either

The `Either` type constructor has the kind `* -> * -> *`. This means that Either takes two type parameters and returns a concrete type.

`Either String Int` is a concrete type. It has the kind `*` because both type parameters (`String` and `Int`) are concrete types.

### Recap: Kinds in Haskell

In Haskell, types are classified into different kinds. A kind can be thought of as a type of a type, describing the number of type parameters a type constructor takes and how they are applied. The Either type has an interesting kind, which we'll explore in detail.

Before diving into the `Either` typeclass, let's briefly recap what kinds are:

`*` (pronounced "star") represents the kind of all concrete types. For example, `Int` and `Bool` have the kind `*`.
`*` -> * represents the kind of type constructors that take one type parameter and return a concrete type. For example, `Maybe` and `[]` (the list type constructor) have the kind `* -> *`.
`* -> * -> *` represents the kind of type constructors that take two type parameters and return a concrete type. For example, Either and (,) (the tuple type constructor) have the kind `* -> * -> *`.

## Typeclasses: Functor, Applicative, and Monad

### Functor

The `Functor` type class expects a type of kind `* -> *`. `For Either`, this means partially applying the first type parameter, e.g., `instance Functor (Either a)`, where `a` will be the type of the `Left`.

We can then define, `fmap` over either, considering `Left` as the error case, and applying the function, when we have a correct (`Right`) case.

```haskell
instance Functor (Either a) where
    fmap _ (Left x)  = Left x
    fmap f (Right y) = Right (f y)
```

An example of using this will be:

```haskell
fmap (+1) (Right 2) -- Result: Right 3
(+1) <$> (Right 2) -- or using infix <$>
fmap (+1) (Left "Error") -- Result: Left "Error"
```

### Applicative

The Applicative type class allows for function application lifted over wrapped values.

In this instance, `pure` wraps a value in `Right`, and `<*>` applies the function inside a `Right` to another `Right`, propagating `Left` values unchanged.

```haskell
instance Applicative (Either a) where
    pure = Right
    Left x <*> _ = Left x
    Right f <*> r = fmap f r
```

```haskell
pure (+1) <*> Right 2 -- Result: Right 3
pure (+1) <*> Left "Error" -- Result: Left "Error"
Right (+1) <*> Right 2 -- Result: Right 3
Right (+1) <*> Left "Error" -- Result: Left "Error"
Left "Error" <*> Right 2 -- Result: Left "Error"
```

### Monad

The Monad type class allows for chaining operations that produce wrapped values.

This involves defining the methods `return` (which should be identical to `pure`) and `>>=` (bind).

```haskell
instance Monad (Either a) where
    return = Right
    (>>=) (Left x) _ = Left x
    (>>=) (Right y) f = f y
```

```haskell
Right 3 >>= (\x -> Right (x + 1)) -- Result: Right 4
Left "Error" >>= (\x -> Right (x + 1)) -- Result: Left "Error"
Right 3 >>= (\x -> Left "Something went wrong") -- Result: Left "Something went wrong"
```

## Example

First, we'll define custom error types to represent possible failures at each stage.

```haskell
data FileError = FileNotFound | FileReadError deriving (Show)
data readError = readError String deriving (Show)
data TransformError = TransformError String deriving (Show)
```

Define a function to read data from a file. If reading succeeds, it returns a `Right` with the file contents, otherwise, it returns a `Left` with a `FileError`.

```haskell
import System.IO (readFile)
import Control.Exception (catch, IOException)

readFileSafe :: FilePath -> IO (Either FileError String)
-- catch any IOException, and use `handleError` on IOException
readFileSafe path = catch (Right <$> (readFile path)) handleError
  where
    handleError :: IOException -> IO (Either FileError String)
    handleError _ = return $ Left FileReadError
```

Define a function to split the file content in to separate lines, if it exists. It returns a `Right` with the read data or a `Left` with a `readError`.

```haskell
readData :: String -> Either readError [String]
readData content
    | null content = Left $ readError "Empty file content"
    | otherwise = Right $ lines content

```

Define a function to transform the read data. It returns a `Right` with transformed data or a `Left` with a `TransformError`.

```haskell
transformData :: [String] -> Either TransformError [String]
transformData lines
    | null lines = Left $ TransformError "No lines to transform"
    -- Simple transformation, where, we reverse each line.
    | otherwise = Right $ map reverse lines
```

The outer `do` block, is using the `IO` monad, while the inner `do` block is using the `Either` monad. This code looks very much like imperative code, using the power of monad to allow for sequencing of operations. However, this is powerful, as it will allow the `Left` error to be threaded through the monadic `do` block, with the user not needing to handle the threading of the error state.

```haskell
main :: IO ()
main = do
    -- Attempt to read the file
    fileResult <- readFileSafe "example.txt"
    
    let result = do
            -- Use monad instance to compute sequential operations
            content <- fileResult
            readData <- readData content
            transformData readdData
    print result
```
