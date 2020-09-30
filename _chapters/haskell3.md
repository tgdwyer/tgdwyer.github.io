---
layout: page
title: "Functor and Applicative"
permalink: /haskell3/
---

In this chapter we see how the Haskell language features we introduced in previous chapters (from function application rules based on [Lambda Calculus](/lambdacalculus) to [Typeclasses](/haskell2#typeclasses)) lead to highly flexible and refactorable code and powerful abstractions.

## Learning Outcomes

- Understand how [eta-conversion](/haskell3#eta-conversion), [operator sectioning](/haskell3#operator-sectioning) and [compose](/haskell3#compose), together provide the ability to transform code to achieve a composable [point free](/haskell3#point-free-code) form and use this technique to refactor code.
- Understand that in Haskell the ability to map over container structures is generalised into the [Functor](/haskell3#functor) typeclass, such that any type that is an instance of Functor has the `fmap` or `(<$>)` operation.
- Understand that the [Applicative Typeclass](/haskell3#applicative) extends Functor such that containers of functions may be applied (using the `(<*>)` operator) to containers of values.

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
       ≡ ((+) x) y  -- making function application precedence explicit
       ≡ (x+) y     -- binary operators can also be partially applied
```
Such operator sectioning allows us to get the right-most parameter of the function on it's own at the right-hand side of the body expression such that we can apply Eta conversion, thus:
```haskell
f x = 1 + x
f x = (1+) x
f = (1+)             -- Eta conversion
```

### Compose

Has its own operator in haskell `(.)`, inspired by the mathematical function composition symbol `∘`:
```haskell
 (f ∘ g) (x) ≡ f (g(x)) -- math notation
 (f . g) x ≡ f (g x)    -- haskell
```
Again, this gives us another way to get the right-most parameter on it's own outside the body expression:
```haskell
f x = sqrt (1 / x)
f x = sqrt ((1/) x)     -- operator section
f x = (sqrt . (1/)) x   -- by the definition of composition
f = sqrt . (1/)         -- eta conversion
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
Prelude> :i Functor
type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
instance Functor [] -- naturally lists are an instance
instance Functor Maybe -- but this may surprise!
... -- and some other instances we'll talk about shortly
```

The first line says that instances of the Functor typeclass must be over types that have the [kind](/haskell2/type-kinds) `(* -> *)`, that is, their constructors must be parameterised with a single type variable.  After this, the `class` definition specifies `fmap` as a function that will be available to any instance of Functor and that `f` is the type parameter for the constructor function, which again, takes one type parameter, e.g. `f a` as the input to `fmap`, which returns an `f b`.

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
...
instance Functor (Either a) -- Defined in `Data.Either'
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

Applicative is a “subclass” of Functor, meaning that an instance of `Applicative` can be ‘`fmap`ed over, but Applicatives also declare (at least) two additional functions, `pure` and `(<*>)` (pronounced ‘apply’ - but I like calling it [“TIE Fighter”](https://en.wikipedia.org/wiki/TIE_fighter)):

```haskell
GHCi> :i Applicative
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b 
...
```

As for `Functor` all instances of the Applicative type-class must satisfy certain laws, which again, are not checked by the compiler.
Applicative instances must comply with these laws to produce the expected behaviour.
You can [read about the Applicative Laws](https://en.wikibooks.org/wiki/Haskell/Applicative_functors)
if you are interested, but they are a little more subtle than the basic Functor laws above, and it is
not essential to understand them to be able to use `<*>` in Haskell.

As for `Functor`, many Base Haskell types are also `Applicative`, e.g. `[]`, `Maybe`, `IO` and `(->)`.

For example, a function inside a `Maybe` can be applied to a value in a `Maybe`.

```haskell
GHCi> Just (+3) <*> Just 2
Just 5 
```

Or a list of functions `[(+1),(+2)]` ), to things inside a similar context (e.g. a list `[1,2,3]`).
```haskell
> [(+1),(+2)] <*> [1,2,3]
[2,3,4,3,4,5] 
```

Note that lists definition of `<*>` produces the cartesian product of the two lists, that is, all the possible ways to apply the functions in the left list, to the values in the right list.  It is interesting to look at [the source](https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.Base.html#Applicative) for the definition of `Applicative` for lists on Hackage:

```haskell
instance Applicative [] where
  pure x    = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]  -- list comprehension 
```

The definition of `<*>` for lists uses a list comprehension.  List comprehensions are a short-hand way to generate lists, using notation similar to mathematical ["set builder notation"](https://en.wikipedia.org/wiki/Set-builder_notation).  The set builder notation here would be:  `{f(x) |  f ∈ fs ∧ x ∈ xs}`.  In English it means: “the set (Haskell list) of all functions in `fs` applied to all values in `xs”`. 


A common use-case for Applicative is applying a binary (two-parameter) function over two Applicative values, e.g.:

```haskell
> pure (+) <*> Just 3 <*> Just 2
Just 5 
```

So:
- `pure` puts the binary function into the applicative (i.e. `pure (+) :: Maybe (Num -> Num -> Num)`), 
- then `<*>`ing this function inside over the first `Maybe` value `Just 3` achieves a partial application of the function inside the `Maybe`. This gives a unary function inside a `Maybe`: i.e. `Just (3+) :: Maybe (Num->Num)`. 
- Finally, we `<*>` this function inside a `maybe` over the remaining `Maybe` value.

This is where the name "applicative" comes from, i.e. `Applicative` is a type over which a non-unary function may be applied.  Note, that the following use of `fmap` is equivalent and a little bit more concise:

```haskell
> (+) <$> Just 3 <*> Just 2
Just 5 
```

This is also called “lifting” a function over an Applicative.  Actually, it’s so common that Applicative also defines dedicated functions for lifting binary functions (in the GHC.Base module):

```haskell
> GHC.Base.liftA2 (+) (Just 3) (Just 2)
Just 5 
```

Here's a little visual summary of Applicative and lifting (box metaphor inspired by [adit.io](http://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)):

![Applicative Visual Summary](/haskell3/applicativepicture.png)

It’s also useful to lift binary data constructors over two Applicative values, e.g. for tuples:
```haskell
> (,) <$> Just 3 <*> Just 2
Just (3, 2) 
```

We can equally well apply functions with more than two arguments over the correct number of values inside Applicative contexts.
Recall our student data type:

```haskell
data Student = Student { id::Integer, name::String, mark::Int } 
```

with a ternary constructor:

```haskell
Student::Integer -> String -> Int -> Student
```

Let's say we want to create a student for a given `id` but we need to look up the name and mark from tables, i.e. lists of key-value pairs: 
`names::[(Integer,String)]` and `marks::[(Integer,Int)]`.  We'll use the function `lookup :: Eq a => a -> [(a, b)] -> Maybe b`, which will either succeed with `Just` the result, or fail to find the value for a given key, and return `Nothing`.

```haskell
lookupStudent :: Integer -> Maybe Student
lookupStudent sid = Student sid <$> lookup sid names <*> lookup sid marks
```

What if `sid` is also the result of some computation that may fail, and is therefore itself wrapped in a `Maybe` context?  Then we will need to apply the ternary `Student` constructor over three `Maybe`s:

```haskell
lookupStudent :: Maybe Integer -> Maybe Student
lookupStudent sid = Student <$> sid <*> lookup sid names <*> lookup sid marks
```

So we can see `<*>` may be chained as many times as necessary to cover all the arguments.

Lists are also instances of `Applicative`.  Given the following types:

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

Or, since both `Suit` and `Rank` derive `Enum`, we can enumerate the full lists of `Suit`s and `Rank`s, and then lift the `Card` operator over both lists to create a whole deck:

```haskell
GHCi> Card <$> [Spade ..] <*> [Two ..]
[Card ^ Two,Card ^ Three,Card ^ Four,Card ^ Five,Card ^ Six,Card ^ Seven,Card ^ Eight,Card ^ Nine,Card ^ Ten,Card ^ Jack,Card ^ Queen,Card ^ King,Card ^ Ace,Card & Two,Card & Three,Card & Four,Card & Five,Card & Six,Card & Seven,Card & Eight,Card & Nine,Card & Ten,Card & Jack,Card & Queen,Card & King,Card & Ace,Card O Two,Card O Three,Card O Four,Card O Five,Card O Six,Card O Seven,Card O Eight,Card O Nine,Card O Ten,Card O Jack,Card O Queen,Card O King,Card O Ace,Card V Two,Card V Three,Card V Four,Card V Five,Card V Six,Card V Seven,Card V Eight,Card V Nine,Card V Ten,Card V Jack,Card V Queen,Card V King,Card V Ace] 
```

#### Exercise

* Create instances of `Show` for `Rank` and `Card` such that a deck of cards displays much more succinctly, e.g.: `[^2,^3,^4,^5,^6,^7,^8,^9,^10,^J,^Q,^K,^A,&2,&3,&4,&5,&6,&7,&8,&9,&10,&J,&Q,&K,&A,O2,O3,O4,O5,O6,O7,O8,O9,O10,OJ,OQ,OK,OA,V2,V3,V4,V5,V6,V7,V8,V9,V10,VJ,VQ,VK,VA]`
* Try and make the definition of `show` for Rank a one-liner using `zip` and `lookup`.

<div class="cheatsheet" markdown="1">

### Different Ways To Apply Functions Cheatsheet

```haskell
 g x         -- apply function g to argument x
 g $ x       -- apply function g to argument x
 g <$> f x   -- apply function g to argument x which is inside Functor f
 f g <*> f x -- apply function g in Applicative context f to argument x which is also inside f
```
</div>

We saw that functions `(->)` are Functors, such that `(<$>)=(.)`.  There is also an instance of Applicative for functions of input type `r`.  We'll give the types of the essential functions for the instance:

```haskell
instance Applicative ((->)r) where
  pure :: a -> (r->a)
  (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
```

This is very convenient for creating pointfree implementations of functions which operate on their parameters more than once.  For example, imagine our `Student` type from above has additional fields with breakdowns of marks: e.g. `exam` and `nonExam`, requiring a function to compute the total mark:

```haskell
totalMark :: Student -> Int
totalMark s = exam s + nonExam s
```

Here's the point-free version, taking advantage of the fact that `exam` and `nonExam`, both being functions of the same input type `Student`, are both in the same Applicative context:

```haskell
totalMark = (+) <$> exam <*> nonExam
```

Or equivalently:

```haskell
totalMark = (+) . exam <*> nonExam
```

---------

### Exercise

- derive the implementations of `pure` and `<*>` for `Maybe` and for functions `((->)r)`.

(hint, if you get stuck there are spoilers in the source from GHC.Base that I linked above)

------------
