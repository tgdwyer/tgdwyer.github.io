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

## Higher-Order Functions

Functions that [take other functions as parameters](/javascript1#functions-as-parameters-to-other-functions) or which [return functions](/javascript1#closures) are called *higher-order functions*.

We've seen many examples of functions which take functions as parameters, for example, operations on arrays:

```javascript
[1,2,3].map(x=>x*x)
```
>[1,4,9]

And we also saw a simple example of a function which returns a new function:

```javascript
const add = x => y => x + y
const add9 = add(9)

add9(3)
add9(1)
```
>12
>10

They are called "higher-order" because they are functions which operate on other functions.
Higher-order functions are a very powerful feature and central to the functional programming paradigm.  

Being able to pass functions into other functions enables code customisability and reuse.  For example, a sort function which allows the caller to pass in a comparison function can easily be made to sort in increasing or decreasing order, or to sort data elements on an arbitrary attribute.

Functions that can create new functions gives rise to all sorts of emergent power, such as the ability to customise, compose and combine functions in very useful ways.  We will see this later when we look at [function composition](/higherorderfunctions#composition) and [combinators](/higherorderfunctions#combinators).

## Curried Functions
Higher-order functions which take a single parameter and return another function operating on a single parameter are called *curried functions*.  The `add` function above is one example.  You can either call it twice immediately to operate on two parameters:

```javascript
add(3)(2)
```
>5

or call it once, leaving one parameter left unspecified, to create a reusable function, like `add9` above.

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

```javascript
function curry2(f) {
   return x=>y=>f(x,y)
}
```
Now, given a function like `plus = (x,y) => x + y`, we can create the curried add function above, like so:
```javascript
const add = curry2(plus)
add(3)(4)
```
> 7

We can also create curried versions of functions with more than two variables:
```javascript
function curry3(f) {
   return x=>y=>z=>f(x,y,z)
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
In order to wrap a value in a function that can be passed to other functions expecting an accessor function as input.
For mocking in tests
For extracting data from encapsulated types (e.g. by passing identity into map).

### K-Combinator

The curried K-Combinator looks like:

```javascript
const K = x=> y=> x
```

So it is a function that ignores its second argument and returns its first argument directly.  Note the similarity to the head function of our [cons list](/functionaljavascript#computation-with-pure-functions).  In fact, we can derive both the head and tail functions used earlier from K and I combinators:

```javascript
const
   K = x=> y=> x,
   I = x=> x,
   cons = x=> y=> f=> f(x)(y),
   head = l=> l(K),
   tail = l=> l(K(I)),
   l = cons(1)(cons(2)(cons(3)(null))),
   forEach = f=> l=> l?(f(head(l)),forEach(f)(tail(l))):null;

forEach(console.log)(l)
```

> 1  
> 2  
> 3

FYI it has been shown that simple combinators like K and I (at least one other is required) are sufficient to create languages as powerful as lambda calculus without the need for lambdas, e.g. see [SKI Combinator Calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus).

Alternation (OR-Combinator)
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

```javascript
function fork(join, f, g) {
   return value => join(f(value), g(value));
}
```

----

## Exercise

* Use the fork-join combinator to compute the average over a sequence of numeric values.

* Add Type annotations to the above definition of the fork function. How many distinct type variables do you need?

----

## Unary

Here’s an interesting example:

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

HINT: parseInt is not actually a unary function.

## Exercises

* From the docs for ```Array.map``` and ```parseInt```  can you figure out why the above is happening?
* Write a function called unary that takes a binary function and a value to bind to its first argument, and returns a unary function.  What is its fully specified TypeScript type signature?
* Flip - e.g. applied to ```map(Iterable,fn)``` to create ```mapApplyFn(Iterable)```. 
