---
layout: chapter
title: "What is a Function?"
---

## Introduction

This chapter serves as a very brief intro/overview of some important and practical aspects of functional programming.

## What is a function?

Think of a standard math function like $f(x)=x+1$ or $\mathit{sin}(x)$. Mathematicians would say that these functions are a mapping from a real number (the domain of the function), to another real number (the codomain of the function).  Then they would use notation like the following to describe any such function $f$ from reals to reals: 

$$f: \mathbb{R} \to \mathbb{R}$$

Most modern programming languages also have a concept of a function.  When learning to program, most computer science students are encouraged to think of a function as a list of instructions, or a "recipe", to use a common metaphor.  Sometimes, these recipes have side effects - such as printing out a result to the console or sending a message to a server, or changing the value of a global variable, or creating dirty pots and pans (to go back to the cooking metaphor).  

In the world of functional programming, we prefer to think of functions more in the mathematical sense of a *pure* transformation from a clearly defined set of possible input parameters, to a clearly defined output. I italicised the word *pure* because, in FP a *pure function* is strictly one that has no side effects: it's only output is it's result value.  

Strict pure functional programming has many desirable properties.  Functions without side effects are easier to reason about and combine, like building blocks, into more complex programs.  But it can be a challenge for students indoctrinated in the *imperative programming* paradigm to relearn how to create programs without changing the values of variables.

## JavaScript functions

JavaScript also uses an arrow notation to define functions, in keeping with the mathematical concept of a function being a transformation.

```javascript
const f = x => x + 1
```

## Higher order functions

Are functions that take another function as parameter, or which return a function.

The `map` function for arrays is an example of a function that takes another function as parameter.

```javascript
[1,2,3,4,5].map(f)
> [2,3,4,5,6]
```

The `filter` function creates a new array omitting values from the input array:

```javascript
const isEven = x => x % 2 === 0
[1,2,3,4,5].filter(x => x % 2 !== 0)
> [1,3,5]
```

An example of a higher-order function which creates a new function as its result, and allows us to reuse functions in different ways:

```javascript
const not = f => x => !f(x)
[1,2,3,4,5].filter(not(isEven))
> [1,3,5]
```

Reduce computes a single value from an imput array:

```javascript
[1,2,3,4,5].reduce(x=>x+1)
> 5
```

### Exercises: `map`, `filter`, and `reduce`

These exercises can be completed in the Chrome Developer Tools console.

Open the console by pressing:

- **Windows/Linux:** `Ctrl` + `Shift` + `J`
- **macOS:** `Command` + `Option` + `J`

For each exercise, replace the comment with an arrow function, then press **Enter** to run the expression.

#### 1. Double every number with `map`

Use `map` to transform each number into twice its original value.

```javascript
[1, 2, 3, 4, 5].map(/* your function here */)
```

Expected result:

```javascript
[2, 4, 6, 8, 10]
```

#### 2. Find word lengths with `map`

Use `map` to replace each word with the number of letters it contains.

```javascript
["cat", "elephant", "mouse"].map(/* your function here */)
```

Expected result:

```javascript
[3, 8, 5]
```

#### 3. Keep the even numbers with `filter`

Use `filter` to keep only the numbers divisible by two.

```javascript
[1, 2, 3, 4, 5, 6].filter(/* your function here */)
```

Expected result:

```javascript
[2, 4, 6]
```

#### 4. Find the passing marks with `filter`

Assume that a mark of 50 or more is a pass. Use `filter` to create an array containing only the passing marks.

```javascript
[37, 82, 49, 50, 68, 91].filter(/* your function here */)
```

Expected result:

```javascript
[82, 50, 68, 91]
```

#### 5. Add the numbers with `reduce`

Use `reduce` to calculate the total of all the numbers. The `0` is the starting value of the total.

```javascript
[4, 7, 2, 9].reduce(/* your function here */, 0)
```

Expected result:

```javascript
22
```

#### 6. Find the largest number with `reduce`

Use `reduce` to find the largest number in the array.

```javascript
[12, 7, 25, 4, 18].reduce(/* your function here */, 0)
```

Expected result:

```javascript
25
```

#### 7. Combine `filter`, `map`, and `reduce`

A player has the following scores:

```javascript
[5, -2, 8, 0, 3]
```

Complete the expression so that it:

1. Removes scores of zero or less.
2. Adds 10 bonus points to each remaining score.
3. Adds the new scores together.

```javascript
[5, -2, 8, 0, 3]
  .filter(/* keep positive scores */)
  .map(/* add 10 to each score */)
  .reduce(/* add the scores */, 0)
```

Expected result:

```javascript
46
```

#### Challenge

Starting with the following temperatures, keep only temperatures above 20, convert them from degrees Celsius to degrees Fahrenheit, and add them together.

To convert Celsius to Fahrenheit, use:

$$F = C \times \frac{9}{5} + 32$$

```javascript
[12, 25, 18, 30, 21]
  .filter(/* keep temperatures above 20 */)
  .map(/* convert Celsius to Fahrenheit */)
  .reduce(/* add the Fahrenheit temperatures */, 0)
```

Expected result:

```javascript
212.8
```

### Solutions

Here are some possible solutions. Other solutions may also work.

```javascript
// Exercise 1
[1, 2, 3, 4, 5].map(x => x * 2)

// Exercise 2
["cat", "elephant", "mouse"].map(word => word.length)

// Exercise 3
[1, 2, 3, 4, 5, 6].filter(x => x % 2 === 0)

// Exercise 4
[37, 82, 49, 50, 68, 91].filter(mark => mark >= 50)

// Exercise 5
[4, 7, 2, 9].reduce((total, x) => total + x, 0)

// Exercise 6
[12, 7, 25, 4, 18].reduce(
  (largest, x) => x > largest ? x : largest,
  0
)

// Exercise 7
[5, -2, 8, 0, 3]
  .filter(x => x > 0)
  .map(x => x + 10)
  .reduce((total, x) => total + x, 0)

// Challenge
[12, 25, 18, 30, 21]
  .filter(celsius => celsius > 20)
  .map(celsius => celsius * 9 / 5 + 32)
  .reduce((total, fahrenheit) => total + fahrenheit, 0)
```

## Curried Functions

A curried `plus` function is an example of the other type of higher-order function, one that returns a new function.

```javascript
const plus = x=>y=>x+y
const plusOne = plus(1)
plusOne(2)
> 3
```
