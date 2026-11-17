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

Strict pure functional programming has many desirable properties.  Functions without side effects are easier to reason about and combine, like building blocks, into more complex programs.   

## JavaScript functions

JavaScript also uses an arrow notation to define functions, in keeping with the mathematical concept of a function being a transformation.

```javascript
const f = x => x + 1
```

## Higher order functions

Are functions that take another function as parameter, or which return a function.

The `map` function for arrays is an example of the former.

```javascript
[1,2,3].map(f)
> [2,3,4]
```

A curried `plus` function is an example of the latter.

```javascript
const plus = x=>y=>x+y
const plusOne = plus(1)
plusOne(2)
> 3
```