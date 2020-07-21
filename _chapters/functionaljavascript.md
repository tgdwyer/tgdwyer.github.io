---
layout: page
title: "Functional Programming in JavaScript"
permalink: /functionaljavascript/
---

### Learning Outcomes

* Create programs in JavaScript in a functional style
* Explain the role of pure functional programming style in managing side effects
* See how pure functions can be used to [model sophisticated computation](#computation-with-pure-functions)

## Introduction

The elements of JavaScript covered in [our introduction](../javascript1), specifically:

* Anonymous functions
* Binding functions to variables
* Higher-order functions

are sufficient for us to explore a paradigm called functional programming.  In the functional programming paradigm the primary model of computation is through the evaluation of functions.  While JavaScript (and many---but not all, as we shall see---other languages inspired by the functional paradigm) do not enforce it, true functional programming mandates the functions be pure in the sense of not causing side effects.  Side effects are changes to state outside of the result returned by the function directly.

# Function Purity vs Side Effects

A *pure function*:

* has no *side effects*: i.e. it has no effects other than to create a return value;
* always produces the same result for the same input.

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

* it mutates the state of the count variable messagesSent from the enclosing scope
* it (likely) does something (what exactly is unclear) to recipient
* finally, it sends output to the console.  Outputting to a device (although only for display purposes) is definitely a side effect.

Side effects are bad for transparency (knowing everything about what a function is going to do) and maintainability.  When state in your program is being changed from all over the place bugs become very difficult to track down.

## Functional Patterns
Passing functions around, anonymous or not, is incredibly useful and pops up in many practical programming situations.

### Eliminating Loops
Loops are the source of many bugs: fence-post errors, range errors, typos, incrementing the wrong counter, etc.

A typical for loop has four distinct places where it’s easy to make errors that can cause critical problems:

```javascript
for ([initialization]; [condition]; [final-expression])
   statement
```

* The **initialization** can initialise to the wrong value (e.g. n instead of n-1, 1 instead of 0) or initialise the wrong variable.
* The **condition** test can use = instead of ==, <= instead of < or test the wrong variable, etc.
* The **final-expression** can (again) increment the wrong variable
* The **statement** body might change the state of variables being tested in the termination condition since they are in scope.

For many standard loops, however, the logic is the same every time and can easily be abstracted into a function.  Examples: Array.map, Array.reduce, Array.forEach, etc.  The logic of the loop body is specified with a function which can execute in its own scope, without the risk of breaking the loop logic.

### Callbacks
In JavaScript and HTML5 events trigger actions associated with all mouse clicks and other interactions with the page.  You subscribe to an event on a given HTML element as follows:
```javascript
element.addEventHandler('click', 
e=>{
// do something when the event occurs, 
// maybe using the result of the event e
})
```

Note that callback functions passed as event handlers are a situation where the difference between the arrow syntax and regular anonymous function syntax really matters.  In the body of the arrow function above this will be bound to the context of the caller, which is probably what you want if you are coding a class for a reusable component.  

### Continuations

Continuations are functions which, instead of returning the result of a computation directly to the caller, pass the result on to another function, specified by the caller.  
We can rewrite basically any function to pass their result to a continuation function instead of returning the result directly.

```javascript
function simplePlus(a, b) {
   return a + b;
}
function continuationPlus(a, b, done:(result)=>void) {
   done(a+b);
}
```

We can also rewrite tail-recursive functions to end with continuations, which specify some custom action to perform when the recursion is complete:

```javascript
function tailRecFactorial(a, n) {
   return n<=1 ? a : tailRecFactorial(n*a, n-1);
}
function continuationFactorial(
a, n, finalAction: (result)=>void): void 
{
   if (n<=1) finalAction(a);
   else continuationFactorial(n*a, n-1, finalAction);
}
```

Continuations are essential in asynchronous processing, because the function will return immediately after dispatching the job, e.g. to the JavaScript event loop:

```javascript
setTimeout(()=>console.log('done.'), 0);
// the above tells the event loop to execute 
// the continuation after 0 milliseconds delay.
// even with a zero-length delay, the synchronous code
// after the setTimeout will be run first...
console.log('job queued on the event loop...');
```

> job queued on the event loop...  
> done.

### Method Chaining

*Chained functions* are a common pattern.  Assuming definitions for map, filter and take similar to:

```javascript
// maps an Iterable (an interface like our IListNode with a way to get a value of 
// the current node or iterate to the next element) containing elements of 
// type T, to an Iterable with elements of type V 
function map<T,V>(f: (_:T)=>V, l: Iterable<T>): Iterable<V> { …

// create a new Iterable containing only the elements of l that 
// satisfy the predicate f

function filter<T>(f: (t:T)=>boolean, l: Iterable<T>): Iterable<T> { …
// create a new Iterable containing only the fist n elements of l

function take<T>(n: number, l: Iterable<T>): Iterable<T> { ...
```

We can chain calls to these functions like so:

```javascript
take(3,
   filter(x=>x%2===0,
       map(x=>x+1, someIterable)
   )
)
```

But in the function call chain above you have to read them inside-out to understand the flow.  Also, keeping track of how many brackets to close gets a bit annoying.  Thus, you will often see class definitions that allow for method chaining, by providing methods that return an instance of the class itself, or another chainable class.

```javascript
class List<T> {
   map<V>(f: (item: T) => V): List<V> {
       return new List(map(f, this.head));
   }
...
```

Then the same flow as above is possible without the nesting and can be read left-to-right, top-to-bottom:

```javascript
someIterable
   .map(x=>x+1)
   .filter(x=>x%2===0)
   .take(3)
```

This is called “fluent” programming style.

## Fluent Interfaces (pure vs impure)

Interfaces like the above in object-oriented languages are often called fluent interfaces.  One thing to be careful about fluent interfaces in JavaScript is that the methods may or may not be pure.  That is, the type system does not warn you whether the method mutates the object upon which it is invoked and simply returns this, or creates a new object, leaving the original object untouched.  We can see,  however, that List.map as defined above, creates a new list and is pure.

### Exercise

* If ```someIterable``` above were declared const, would it protect you against mutations in ```someIterable``` due to impure methods?

## Computation with Pure Functions

Pure functions may seem restrictive, but in fact pure function expressions and higher-order functions can be combined into powerful programs.  In fact, anything you can compute with an imperative program can be computed through function composition.  Side effects are required eventually, but they can be managed and the places they occur can be isolated.  Let’s do a little demonstration, although it might be a bit impractical, we’ll make a little list processing environment with just functions:

```javascript
const cons = (head, rest)=> selector=> selector(head, rest);
```

With just the above definition we can construct a list (the term cons dates back to LISP) with three elements, terminated with null, like so:

```javascript
const list123 = cons(1, cons(2, cons(3, null)));
```

The data element, and the reference to the next node in the list are stored in the closure returned by the ```cons``` function.  Created like this, the only side-effect of growing the list is creation of new cons closures.  Mutation of more complex structures such as trees can be managed in a similarly ‘pure’ way, and surprisingly efficiently, as we will see later in this course. 

So cons is a function that takes two parameters (```head``` and ```rest```), and returns a function that itself takes a function (selector) as argument.  The selector function is then applied to ```head``` and ```rest```.  What might the selector function be and how do we apply it to a list element?  Well we don’t exactly apply it ourselves, we give it to the closure returned by the ```cons``` function and it applies it for us.  There are the two selectors we need to work with the list:

```javascript
const
    head = list=> list((head, rest)=> head),
    rest = list=> list((head, rest)=> rest);
```

Now, ```head``` gives us the first data element from the list, and rest gives us another list.  Now we can access things in the list like so:

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

In the above, we are using closures to store data.  It's just a trick to show the power of functions and to into the right state of mind for the Lambda Calculus - which provides a complete model of computation using only anonymous functions like those above.  In a real program I would expect you would use JavaScript's class and object facilities to create data structures.

### Towards Lambda Calculus and Church Encoding

Thus, with only pure function expressions and JavaScript conditional expressions (```?:```) we can begin to perform complex computations.  We can actually go further and eliminate the conditional expressions with more functions! Here’s the gist of it: we wrap list nodes with another function of two arguments, one argument, ```whenempty```, is a function to apply when the list is empty, the other argument, ```notempty```, is applied by all internal nodes in the list.  An empty list node (instead of null) applies the ```whenempty``` function when visited, a non-empty node applies the ```notempty``` function. The implementations of each of these functions then form the two conditions to be handled by a recursive algorithm like ```map``` or ```reduce```.  See [“Making Data out of Functions” by Braithwaite](https://leanpub.com/javascriptallongesix/read#leanpub-auto-making-data-out-of-functions) for a more detailed exposition of this idea.

These ideas, of computation through pure function expressions, are inspired by Alonzo Church’s *lambda calculus*.   We’ll be looking again at the lambda calculus later.  Obviously, for the program to be at all useful you will need some sort of side effect, such as outputting the results of a computation to a display device.  When we begin to explore PureScript and Haskell later in this course we will discuss how such languages manage this trick while remaining “pure”.

---------------

## Exercises

- Implement a ```fromArray``` function to construct a ```cons``` list from an array
- Implement a ```filter``` function, which takes a function and a cons list, and returns another cons list populated only with those elements of the list for which the function returns true
- Implement a ```reduce``` function for these cons lists, similar to javascript’s ```Array.reduce```
- Implement a ```concat``` function that takes two lists as arguments and returns a new list of their concatenation.
- How can we update just one element in this list without mutating any data and what is the run-time complexity of such an operation?

-------------

## Updating Data Structures With Pure Functions

We saw in the [introduction to JavaScript](javascript1) that one can create objects with a straightforward chunk of JSON:

```javascript
const studentVersion1 = {
  name: "Tim",
  assignmentMark: 20,
  examMark: 15
}
```

> studentVersion1  
> {name: "Tim", assignmentMark: 20, examMark: 15}

Conveniently, one can copy all of the properties from an existing object into a new object using the "spread" operator ```...```, followed by more JSON properties that can potentially overwrite those of the original.  For example, the following creates a new object with all the properties of the first, but with a different assignmentMark:

```javascript
const studentVersion2 = {
    ...studentVersion1,
    assignmentMark: 19
}
```

> studentVersion2  
> {name: "Tim", assignmentMark: 19, examMark: 15}

One can encapsulate such updates in a succinct pure function:

```javascript
function updateExamMark(student, newMark) {
    return {...student, examMark: newMark}
}

const studentVersion3 = updateExamMark(studentVersion2, 19)
```

> studentVersion3  
> {name: "Tim", assignmentMark: 19, examMark: 19}

Note that when we declared each of the variables ```studentVersion1-3``` as ```const```, these variables are only constant in the sense that the object reference cannot be changed.  That is, they cannot be reassigned to refer to different objects:

```javascript
studentVersion1 = studentVersion2
```

> VM430:1 Uncaught TypeError: Assignment to constant variable.  

However, there is nothing in these definitions to prevent the properties of those objects from being changed:

```javascript
studentVersion1.name = "Tom"
```

> studentVersion1  
> {name: "Tom", assignmentMark: 20, examMark: 15}

We will see later how the [TypeScript compiler](../typescript1) allows us to create deeply immutable objects that will trigger compile errors if we try to change their properties.

You may wonder how pure functions can be efficient if the only way to mutate data structures is by returning a modified copy of the original.  There are two responses to such a question, one is: "purity helps us avoid errors in state management through wanton mutation effects - in modern programming correctness is often a bigger concern than efficiency", the other is "properly structured data permits log(n) time copy-updates, which should be good enough for most purposes".  We'll explore what is meant by the latter in later sections of these notes.


