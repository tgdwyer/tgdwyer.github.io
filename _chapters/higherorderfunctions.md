---
layout: page
title: "Higher Order Functions"
permalink: /higherorderfunctions/
---


## Learning Outcomes


## Introduction

The really exciting aspect of higher-order function support in languages like JavaScript is that it allows us to combine simple reusable functions in sophisticated ways.  We’ve already seen how functions like map, filter and reduce can be chained to flatten the control flow of data processing.  In this section we will look at some tricks that allow us to use functions that work with other functions in convenient ways.

## Curried Functions

Let’s say we want a function for computing the volume of cylinders, parameterised by the approximation for  that we plan to use:

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

* Create a ```compose``` function in javascript that takes a variable number of functions as arguments and composes (chains) them.  Using the spread operator (...) to take a variable number of arguments as an array and the Array.prototype.reduce method, the function should be very small.  Note that you won’t be able to give this a satisfactory type in TypeScript until TypeScript supports [variadic kinds](https://github.com/Microsoft/TypeScript/issues/5453).
