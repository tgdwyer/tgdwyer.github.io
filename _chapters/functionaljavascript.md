---
layout: page
title: "Functional Programming in JavaScript"
permalink: /functionaljavascript/
---

### Learning Outcomes

* Create programs in JavaScript in a functional style
* Explain the role of pure functional programming style in managing side effects

## Introduction

The elements of JavaScript covered in [our introduction](../javascript1), specifically:

* Anonymous functions
* Binding functions to variables
* Higher-order functions

are sufficient for us to explore a paradigm called functional programming.  In the functional programming paradigm the primary model of computation is through the evaluation of functions.  While JavaScript (and many---but not all, as we shall see---other languages inspired by the functional paradigm) do not enforce it, true functional programming mandates the functions be pure in the sense of not causing side effects.  Side effects are changes to state outside of the result returned by the function directly.

# Function Purity vs Side Effects
Pure functions are perhaps most easily illustrated with some examples and counterexamples. The following function has no effect outside its return result and is therefore pure:

```javascript
function squares(a) {
    let b = new Array(a.length);
    for (let i = 0; i < a.length; i++) {
        b[i] = a[i]**2;
    }
    return b;
}
```

While the above function (viewed as a black box) is pure, its implementation is not very functional.  Specifically, the code around variable b does not have the property of referential transparency.  That is, because the value of b is reassigned during execution figuring out the state at a given line is impossible, without looking at its context. True functional languages enforce referential transparency through immutable variables (which sounds like a tautology).  That is, once a variable is bound to a value, it cannot be reassigned.  In JavaScript we can opt-in to immutable variables by declaring them const.
 
A more functional way to implement the squares function would be more like the examples we have seen previously:

```javascript
const squares = a=> a.map(x=> x**2)
```

By contrast, the following is considered impure because it modifies the memory referred to by a in-place:

```javascript
function squares(a) {
    for (let i = 0; i < a.length; i++) {
        a[i] = a[i]**2;
    }
}
```

An impure function typical of something you may see in OO code:

```javascript
let messagesSent = 0;
function send(message, recipient) {
   let success = recipient.notify(message);
   if (success) {
       ++messagesSent;
   } else {
       console.log("send failed! " + message);
   }
   console.log("messages sent " + messagesSent);
}
```

This function is impure in three ways:
it mutates the state of the count variable messagesSent from the enclosing scope
it (likely) does something (what exactly is unclear) to recipient
finally, it sends output to the console.  Outputting to a device (although only for display purposes) is definitely a side effect.

Side effects are bad for transparency (knowing everything about what a function is going to do) and maintainability.  When state in your program is being changed from all over the place bugs become very difficult to track down.

# Computation with Pure Functions

Pure functions may seem restrictive, but in fact pure function expressions and higher-order functions can be combined into powerful programs.  In fact, anything you can compute with an imperative program can be computed through function composition.  Side effects are required eventually, but they can be managed and the places they occur can be isolated.  Let’s do a little demonstration, although it might be a bit impractical, we’ll make a little list processing environment with just functions:

```javascript
const cons = (head, rest)=> selector=> selector(head, rest);
```

With just the above definition we can construct a list (the term cons dates back to LISP) with three elements, terminated with null, like so:

```javascript
const list123 = cons(1, cons(2, cons(3, null)));
```

The data element, and the reference to the next node in the list are stored in the closure returned by the cons function.  Created like this, the only side-effect of growing the list is creation of new cons closures.  Mutation of more complex structures such as trees can be managed in a similarly ‘pure’ way, and surprisingly efficiently, as we will see later in this course. 

So cons is a function that takes two parameters (head and rest), and returns a function that itself takes a function (selector) as argument.  The selector function is then applied to head and rest.  What might the selector function be and how do we apply it to a list element?  Well we don’t exactly apply it ourselves, we give it to the closure returned by the cons function and it applies it for us.  There are the two selectors we need to work with the list:

```javascript
const   
    head = list=> list((head, rest)=> head),
    rest = list=> list((head, rest)=> rest);
```

Now, head gives us the first data element from the list, and rest gives us another list.  Now we can access things in the list like so:

```javascript
const one = head(list123), // ===1
    list23 = rest(list123),
    two = head(list23), // ===2
    … // and so on
```

Now, here’s the ubiquitous map function:

```javascript
const map = (f, list)=> !list ? null 
: cons(f(head(list)), map(f, rest(list)))
```
---------------

## Exercises

- Implement a fromArray function to construct a list from an array
- Implement a filter function, which takes a function and a list, and returns another list populated only with those elements of the list for which the function returns true
- Implement a reduce function for these functional lists, similar to javascript’s Array.reduce
- Implement a concat function that takes two lists as arguments and returns a new list of their concatenation.
- How can we update just one element in this list without mutating any data and what is the run-time complexity of such an operation?

-------------

# Conclusion

Thus, with only pure function expressions and JavaScript conditional expressions (?:) we can begin to perform complex computations.  We can actually go further and eliminate the conditional expressions with more functions! Here’s the gist of it: we wrap list nodes with another function of two arguments, one argument, whenempty, is a function to apply when the list is empty, the other argument, notempty, is applied by all internal nodes in the list.  An empty list node (instead of null) applies the whenempty function when visited, a non-empty node applies the notempty function. The implementations of each of these functions then form the two conditions to be handled by a recursive algorithm like map or reduce.  See “Making Data out of Functions” by Braithwaite for a more detailed exposition of this idea.

These ideas, of computation through pure function expressions, are inspired by Alonzo Church’s lambda calculus.   We’ll be looking again at the lambda calculus later.  Obviously, for the program to be at all useful you will need some sort of side effect, such as outputting the results of a computation to a display device.  When we begin to explore PureScript and Haskell later in this course we will discuss how such languages manage this trick while remaining “pure”.


