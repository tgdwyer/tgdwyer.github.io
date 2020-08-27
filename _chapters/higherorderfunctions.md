---
layout: page
title: "Higher Order Functions"
permalink: /higherorderfunctions/
---


## Learning Outcomes

- Understand that Higher-Order Functions are those which take other functions as input parameters or which return functions
- Understand that a Combinator is a higher-order function that uses only function application and earlier defined combinators to define a result from its arguments
- Use simple Combinator functions ([identity](#identity-i-combinator)) to manipulate and compose other functions

## Introduction

The really exciting aspect of higher-order function support in languages like JavaScript is that it allows us to combine simple reusable functions in sophisticated ways.  We’ve already seen how functions like map, filter and reduce can be chained to flatten the control flow of data processing.  In this section we will look at some tricks that allow us to use functions that work with other functions in convenient ways.

*A note about type annotations in this section.* If you are following the reading order given by the [index for these notes](/), then you have already read our [introduction to TypeScript](/typescript1/).  Therefore, below we frequently use TypeScript type annotions to be precise about the intended use of the functions.  However, as we start to rely more and more heavily on curried higher-order functions in this chapter, TypeScript type annotations start to become a bit cumbersome, and for the purposes of concisely representing use of combinators to create new functions, we abandon them entirely.  As an exercise, you may like to think about what the TypeScript annotations for some of these functions should be.  This is one of the reasons why we later in these notes move away from JavaScript and TypeScript entirely to instead focus on a real functional language, [Haskell](/haskell1/).

## Higher-Order Functions

Functions that [take other functions as parameters](/javascript1#functions-as-parameters-to-other-functions) or which [return functions](/javascript1#closures) are called *higher-order functions*.
They are called "higher-order" because they are functions which operate on other functions.
Higher-order functions are a very powerful feature and central to the functional programming paradigm.  

We've seen many examples of functions which take functions as parameters, for example, operations on arrays:

```javascript
[1,2,3].map(x=>x*x)
```
>[1,4,9]

Being able to pass functions into other functions enables code customisability and reuse.  For example, a sort function which allows the caller to pass in a comparison function can easily be made to sort in increasing or decreasing order, or to sort data elements on an arbitrary attribute.

And we also saw a simple example of a function which returns a new function:

```javascript
const add = x => y => x + y
const add9 = add(9)

add9(3)
add9(1)
```

>12  
>10

Functions that can create new functions gives rise to all sorts of emergent power, such as the ability to customise, compose and combine functions in very useful ways.  We will see this later when we look at [function composition](/higherorderfunctions#composition) and [combinators](/higherorderfunctions#combinators).

## Curried Functions

Higher-order functions which take a single parameter and return another function operating on a single parameter are called *curried functions*.  The `add` function above is one example.  You can either call it twice immediately to operate on two parameters:

```javascript
add(3)(2)
```
>5

or call it once, leaving one parameter left unspecified, to create a reusable function, like `add9` above.
Such use of a curried function with only a subset of its parameters is called *partial application*.  Partial application returns a function for which further parameters must be supplied before the body of the function can finally be evaluated, e.g.:
```js
add9(1)
```
>10  

Here's a practical example of a curried function, let’s say we want a function for computing the volume of cylinders, parameterised by the approximation for π that we plan to use:

```javascript
function cylinderVolume(pi: number, height: number, radius: number): number {
   return pi * radius * radius * height;
}
```

And we invoke it like so:

```javascript
cylinderVolume(Math.PI, 4, 2);
```

Now consider another version of the same function:

```javascript
function cylinderVolume(pi: number) {
  return function(height: number) {
    return function(radius: number) {
      return pi * radius * radius * height;
    }
  }
}
```

This one, we can invoke like so:

```javascript
cylinderVolume(Math.PI)(4)(2)
```

But we have some other options too.  For example, we are unlikely to change our minds about what precision approximation of PI we are going to use between function calls. So let’s make a local function that fixes PI:

```javascript
const cylVol = cylinderVolume(Math.PI);
```

Which we can invoke when we are ready like so:

```javascript
cylVol(4)(2)
```

What if we want to compute volumes for a whole batch of cylinders of fixed height of varying radii?  

```javascript
const radii = [1.2,3.1,4.5, … ],
      makeHeight5Cylinder = cylVol(5),
      cylinders = radii.map(makeHeight5Cylinder);
```

Or we can make it into a handy function to compute areas of circles:

```javascript
const circleArea = cylVol(1)
```

Such functions are called *curried functions* and they are named after a mathematician named Haskell Curry.  This gives you a hint as to what functions look like in the Haskell programming language and its variants.
We can also create a function to make curried versions of conventional multiparameter javascript functions:

```typescript
function curry2<T,U,V>(f: (x:T,y:U)): V {
   return x=>y=>f(x,y)
}
```

Now, given a function like `plus = (x,y) => x + y`, we can create the curried add function above, like so:

```javascript
const add = curry2(plus)
add(3)(4)
```
> 7

We can also create curried versions of functions with more than two variables - but the TypeScript syntax for functions with arbitrary numbers of arguments gets a bit scary (one of the many reasons we will shortly [switch to Haskell](/haskell1/) for our exploration of more advanced functional programming topics).

```javascript
// Don't try to understand this - this is just to show you why Haskell is better for strictly-typed FP
function curry<T extends unknown[], U extends unknown[], R>(fn: (...ts: [...T, ...U]) => R, ...args:T): (...bs:U) => R {
    return (...bargs: U) => fn(...args, ...bargs);
}
```

## Composition

Consider the following function which takes two functions as input, note the way the types line up:

```javascript
function compose<U,V,W>(f:(x:V)=>W,g:(x:U)=>V) {
  return (x:U)=> f(g(x))
}
```

This function lets us combine two functions into a new reusable function.  For example, given a messy list of strings representing numbers of various precision:

```javascript
const grades = ['80.4','100.000','90','99.25']
```

We can define a function to parse these strings into numbers and then round them to the nearest whole number purely through composition of two existing functions:

```javascript
const roundFloat = compose(Math.round, Number.parseFloat)
```

And then apply it to the whole set:

```javascript
grades.map(roundFloat)
```

> [80, 100, 90, 99]

Note that compose let us define roundFloat without any messing around with anonymous functions and explicit wiring-up of return values to parameters.  We call this *tacit* or *point-free* style programming.

## Exercise

* Create a ```compose``` function in javascript that takes a variable number of functions as arguments and composes (chains) them.  Using the spread operator (...) to take a variable number of arguments as an array and the Array.prototype.reduce method, the function should be very small.

* Create a ```pipe``` function which composes its arguments in the opposite order to the ```compose``` function above.  That is, left-to-right.  Note that in [rx.js](https://www.learnrxjs.io/), such a ```pipe``` function is an important way to create chains of operations (over Observable streams).

## Combinators
Combinators are higher-order functions which perform pure operations on their arguments to perform a result.  They may seem very basic, but as their name suggests, they provide useful building blocks for manipulating and composing functions to create new functions.  The [`compose`](#composition) function is a combinator.  Some more examples follow.

### Identity I-Combinator

The following may seem trivial:

```javascript
function identity<T>(value: T): T {
   return value;
}
```

But it has some important applications:

- In order to wrap a value in a function that can be passed to other functions expecting an accessor function as input.
- For mocking in tests.
- For extracting data from encapsulated types (e.g. by passing identity into map).
- For compositional use with other combinators, as below.

### K-Combinator

The curried K-Combinator looks like:

```javascript
const K = x=> y=> x
```

So it is a function that ignores its second argument and returns its first argument directly.  Note the similarity to the `head` function of our [cons list](/functionaljavascript#computation-with-pure-functions).  In fact, we can derive curried version so both the `head` and `rest` functions used earlier from K and I combinators:

```javascript
const
   K = x=> y=> x,
   I = x=> x,
   cons = x=> y=> f=> f(x)(y),
   head = l=> l(K),
   tail = l=> l(K(I)),
   forEach = f=> l=> l?(f(head(l)),forEach(f)(tail(l))):null;

const l = cons(1)(cons(2)(cons(3)(null)));

forEach(console.log)(l)
```

> 1  
> 2  
> 3

The definition of `head` is by straight-forward, like-for-like substitution of K into a curried version of our previous definition for `head`.  Note, the following is not code, just statements of equivalence (≡):

```
head ≡ l=>l((h,_)=>h) -- previous uncurried definition of head
     ≡ l=>l(curry2((h,_)=>h))
     ≡ l=>l(h=>_=>h)
```

Where the expression in brackets above we notice is equivalent to `K`:

```
K  ≡  x=> y=> x  ≡  h=> _=> h
```

In the context of the [Lambda Calculus](/lambdacalculus), we will see that such a renaming is called *Alpha Conversion*.

We are gradually changing our terminology to be more haskell-like, so we have named the curried version of `rest` to `tail`.  The new definition of `tail`, compared to our previous definition of `rest`, is derived as follows:

```
K(I)  ≡  K(i=> i)             -- expand I := i=> i
      ≡  (x=> y=> x)(i=> i)   -- expand K := x=> y=> x
      ≡  y=> i=> i
```
Where the last line above is the result of applying `x=>y=>x` to `i=>i`.  Thus, we substitute `x:=i=>i` in the body of the first function (the expansion of `K`).  When we explore the [Lambda Calculus](/lambdacalculus), we will see that this operation (simple evaluation of function application by substition of expressions) is called *Beta reduction*.

Now we could derive `tail` from `rest` using our `curry2` function:
``` 
rest ≡ l=>l((_,r)=>r)
tail ≡ l=>l(curry2((_,r)=>r))
     ≡ l=>l(_=>r=>r)
```
Where `_=> r=> r  ≡  y=> i=> i` and therefore `tail ≡ l=>l(K(i))`.  QED!!!

FYI it has been shown that simple combinators like K and I (at least one other is required) are sufficient to create languages as powerful as lambda calculus without the need for lambdas, e.g. see [SKI Combinator Calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus).

In previous sections we have see a number of versions of functions which transform lists (or other containers) into new lists like `map`, `filter` and so on.  We have also introduced the reduce function as a way to compute a single value over a list.  If we realise that the value we produce from reduce can also be a list, we can actually use reduce to implement all of the other lists transformations.  Instead of returning a value from a reduce, we could apply a function which produces only side effects, thus, performing a `forEach`.  We'll use this as an example momentarily.

First, here's another implementation of `reduce` for the above formulation of cons lists - but we rename it `fold` (again, as our JavaScript becomes more and more Haskell like we are beginning to adopt Haskell terminology).

```js
const fold = f=> i=> l=> l ? fold(f)(f(i)(head(l)))(tail(l)) : i
```

Now, for example, we can define `forEach` in terms of `fold`:
```javascript
const forEach = f=>l=>fold(_=>v=>f(v))(null)(l)
```
Now, the function `f` takes one parameter and we don't do anything with its return type (in TypeScript we could enforce the return type to be `void`).
However, `fold` is expecting as its first argument a curried function of two parameters (the accumulator and the list element).  Since in `forEach` we are not actually accumulating a value, we can ignore the first parameter, hence we give `fold` the function `_=>v=>f(v)`, to apply `f` to each value `v` from the list.

But note that `v=>f(v)` is precisely the same as just `f`.
So we can simplify forEach a bit further:

```javascript
const forEach = f=>l=>fold(_=>f)(null)(l)
```

But check out these equivalences:

```
K(f)  ≡  (x=> y=> x)(f) -- expand K
      ≡  y=> f          -- apply the outer function to f, hence we substitute x:= f
      ≡  _=> f          -- rename y to _
```

Where, in the last line above, since y doesn't appear anywhere in the body of the function we don't care what it's called anymore and rename it to `_`.

Therefore, we can use our `K` combinator to entirely avoid defining any functions in the body of `forEach`:

```javascript
const forEach = f=>l=>fold(K(f))(null)(l)
```


----

### Fold Exercise

* Write `map` and `filter` for the above cons list definition in terms of `fold`

----

### Alternation (OR-Combinator)

A function that applies a first function.  If the first function fails (returns undefined, false or null), it applies the second function.  The result is the first function that succeeded.

```javascript
const or = f=> g=> v=> f(v) || g(v)
```

Basically, it’s a curried if-then-else function with continuations. Imagine something like the following data for student names in a unit, then a dictionary of the ids of students in each class:

```javascript
const students = ['tim','sally','sam','cindy'],
      class1 = { 'tim':123, 'cindy':456},
      class2 = { 'sally':234, 'sam':345};
```

We have a function that lets us lookup the id for a student in a particular class:

```javascript
const lookup = class=> name=> class[name]
```

Now we can try to find an id for each student, first from ```class1``` but fall back to ```class2``` if it isn’t there:

```javascript
const ids = students.map(or(lookup(class1))(lookup(class2)))
```

### Fork-Join Combinator

The following is cute:

```javascript
function fork(join, f, g) {
   return value => join(f(value), g(value));
}
```

But we'll leave trying it out as an exercise.

----

### Fork-Join Exercise

* Use the fork-join combinator to compute the average over a sequence of numeric values.

* Add Type annotations to the above definition of the fork function. How many distinct type variables do you need?

----

## End Note

### Unary versus Binary Functions in JavaScript

Uncurried functions of two parameters can be called Binary functions.  Functions of only one parameter can therefore be called Unary functions.  Note that all of our curried functions are unary functions, which return other unary functions.
We've seen situations now where curried functions are flexibly combined to be used in different situations.  

Note that in JavaScript you sometimes see casual calls to binary functions but with only one parameter specified.  Inside the called function the unspecified parameter will simply be `undefined` which is fine if the case of that parameter being `undefined` is expected and handled.  Conversely, we also sometimes see additional parameters passed to unary functions 
But, here’s an interesting example where mixing up unary and binary functions in JavaScript's very forgiving environment can go wrong.

```javascript
['1','2','3'].map(parseInt);
```

We are converting an array of strings into an array of int.  The output will be ```[1,2,3]``` right?  WRONG!

```javascript
['1','2','3'].map(parseInt);
```

> [1, NaN, NaN]

What the ...!

But:

```javascript
parseInt('2')
```

> 2

What's going on?
HINT: [parseInt is not actually a unary function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseInt).

The point of this demonstration is that curried functions are a more principled way to support partial function application, and also much safer and easier to use when the types guard against improper use.  Thus, this is about as sophisticated as we are going to try to get with Functional Programming in JavaScript, and we will pick up our discussion further exploring the power of FP in the context of the [Haskell functional programming language](/haskell1/).  However, libraries do exist that provide quite flexible functional programming abstractions in JavaScript.  For example, you might like to investigate [Ramda](http://ramdajs.com).

## Exercises

* From the docs for ```Array.map``` and ```parseInt```  can you figure out why the above is happening?
* Write a function called unary that takes a binary function and a value to bind to its first argument, and returns a unary function.  What is its fully specified TypeScript type signature?
* Flip - e.g. applied to ```map(Iterable,fn)``` to create ```mapApplyFn(Iterable)```. 

---
