---
layout: chapter
title: "What is a Function?"
---

## Introduction

This chapter serves as a very brief intro/overview of some important and practical aspects of functional programming.  It is both a deep-dive but also glosses over much of the background and scope of the paradigm.  You are not expected to understand everything, but will hopefully come away excited about the concepts and potential.  Please ask questions if you are unsure - don't be shy.  For those who want more, the full course notes are [here](https://tgdwyer.github.io).  [Here](https://stackblitz.com/edit/asteroids2023) is a teaser of where we might get to by the end of the workshop if everything goes smoothly.

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

Share your answers [here](https://codeshare.io/ar08LE)

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

## Functional Reactive Programming with RxJS

So far, we have used functions to transform arrays:

```javascript
[1, 2, 3, 4, 5]
  .filter(x => x % 2 === 0)
  .map(x => x * 2)
```

An array contains values that are already available. However, many programs must respond to values that arrive over time:

- keyboard presses;
- mouse clicks;
- messages from a server;
- measurements from a sensor;
- frames in an animation.

We do not know in advance exactly when these values will arrive. A program is **reactive** when it responds to values or events as they arrive.

[RxJS](https://rxjs.dev/) is a JavaScript library for working with sequences of values called **Observables**. An Observable can represent a fixed collection of values, but it can also represent a stream of future events.

### From arrays to Observables

The following array contains three values:

```javascript
[1, 2, 3]
```

We can turn it into an Observable using `from`:

```javascript
from([1, 2, 3])
```

Instead of producing one array, this Observable emits the values one at a time:

```text
1
2
3
```

We use `subscribe` to provide a function that will be called whenever the Observable emits a value:

```javascript
from([1, 2, 3]).subscribe(console.log)
```

Calling `subscribe` starts the Observable. In this example, `console.log` prints each emitted value.

### Transforming a stream

RxJS provides familiar operations such as `map`, `filter`, and `reduce`. Observable operators are placed inside `pipe`:

```javascript
from([1, 2, 3, 4, 5])
  .pipe(
    filter(x => x % 2 === 0),
    map(x => x * 2)
  )
  .subscribe(console.log)
```

This prints:

```text
4
8
```

The operations have similar purposes for arrays and Observables:

| Array | Observable |
|---|---|
| `.map(f)` | `.pipe(map(f))` |
| `.filter(f)` | `.pipe(filter(f))` |
| `.reduce(f, start)` | `.pipe(reduce(f, start))` |
| produces a result immediately | emits results to a subscriber |

Each operator creates a new Observable rather than changing the original one. This lets us build programs by composing small transformations.

### `reduce` and `scan`

The Observable `reduce` operator behaves like array `reduce`: it waits for the source to finish, then emits its final result.

```javascript
from([1, 2, 3, 4])
  .pipe(
    reduce((total, value) => total + value, 0)
  )
  .subscribe(console.log)
```

This emits only:

```text
10
```

Some streams, such as mouse clicks, might continue indefinitely. For these streams, `scan` is often more useful. It emits every intermediate result:

```javascript
from([1, 2, 3, 4])
  .pipe(
    scan((total, value) => total + value, 0)
  )
  .subscribe(console.log)
```

This emits:

```text
1
3
6
10
```

You can think of `scan` as a running `reduce`.

## Setting up the Chrome console

Open this page in Chrome, then open the Developer Tools console:

- **Windows/Linux:** `Ctrl` + `Shift` + `J`
- **macOS:** `Command` + `Option` + `J`

Paste the following code into the console. Wait until it prints `RxJS is ready`.

```javascript
if (!window.rxjs) {
  await new Promise((resolve, reject) => {
    const script = document.createElement("script")
    script.src =
      "https://unpkg.com/rxjs@7.8.2/dist/bundles/rxjs.umd.min.js"
    script.onload = resolve
    script.onerror = reject
    document.head.appendChild(script)
  })
}

var { from, fromEvent, interval } = rxjs
var { map, filter, reduce, scan, take } = rxjs.operators

console.log("RxJS is ready")
```

The setup uses `var` so that it can safely be run again after an error.

## Worksheet

For each exercise, replace the comments with suitable arrow functions and run the expression in the console.

Remember the general structure:

```javascript
sourceObservable
  .pipe(
    operator(/* a function */),
    operator(/* another function */)
  )
  .subscribe(console.log)
```

### 1. Double every number with `map`

Use the RxJS `map` operator to double each emitted number.

```javascript
from([1, 2, 3, 4, 5])
  .pipe(
    map(/* your function here */)
  )
  .subscribe(console.log)
```

Expected output:

```text
2
4
6
8
10
```

### 2. Find word lengths with `map`

Replace each emitted word with the number of letters it contains.

```javascript
from(["cat", "elephant", "mouse"])
  .pipe(
    map(/* your function here */)
  )
  .subscribe(console.log)
```

Expected output:

```text
3
8
5
```

### 3. Keep the even numbers with `filter`

Allow only even numbers to continue through the stream.

```javascript
from([1, 2, 3, 4, 5, 6])
  .pipe(
    filter(/* your function here */)
  )
  .subscribe(console.log)
```

Expected output:

```text
2
4
6
```

### 4. Find the passing marks with `filter`

Assume that a mark of 50 or more is a pass.

```javascript
from([37, 82, 49, 50, 68, 91])
  .pipe(
    filter(/* your function here */)
  )
  .subscribe(console.log)
```

Expected output:

```text
82
50
68
91
```

### 5. Add the numbers with `reduce`

Use `reduce` to add all the emitted numbers. The final total should be emitted after the source completes.

```javascript
from([4, 7, 2, 9])
  .pipe(
    reduce(/* your function here */, 0)
  )
  .subscribe(console.log)
```

Expected output:

```text
22
```

### 6. Combine `filter`, `map`, and `reduce`

A player has the following scores:

```javascript
[5, -2, 8, 0, 3]
```

Complete the stream so that it:

1. removes scores of zero or less;
2. adds 10 bonus points to each remaining score;
3. adds the new scores together.

```javascript
from([5, -2, 8, 0, 3])
  .pipe(
    filter(/* keep positive scores */),
    map(/* add 10 to each score */),
    reduce(/* add the scores */, 0)
  )
  .subscribe(console.log)
```

Expected output:

```text
46
```

### 7. Create a stream that changes over time

The `interval` function creates an Observable that emits a number at regular intervals. It begins with zero.

The `take(5)` operator allows only five values through, then completes the stream automatically.

Complete the `map` so that the stream counts from 1 to 5 instead of from 0 to 4.

```javascript
interval(1000)
  .pipe(
    take(5),
    map(/* change 0, 1, 2, 3, 4 into 1, 2, 3, 4, 5 */)
  )
  .subscribe(seconds => console.log(`${seconds} second(s)`))
```

Expected output, with one line appearing each second:

```text
1 second(s)
2 second(s)
3 second(s)
4 second(s)
5 second(s)
```

This example is reactive: the subscriber responds whenever the timer emits a new value.

### 8. Count mouse clicks with `scan`

The `fromEvent` function creates an Observable from events on a web page.

Complete the `scan` operator so that it counts mouse clicks. After starting the stream, click five times on the web page itself.

```javascript
fromEvent(document, "click")
  .pipe(
    scan(/* increase the count */, 0),
    take(5)
  )
  .subscribe(count => console.log(`Click number ${count}`))
```

Expected output:

```text
Click number 1
Click number 2
Click number 3
Click number 4
Click number 5
```

Unlike `reduce`, `scan` emits the updated count after every click.

### 9. Keyboard challenge

Create a stream that:

1. listens for keyboard events;
2. extracts the name of each key;
3. keeps only single-character keys;
4. converts each character to uppercase;
5. completes after five characters.

Click the web page after starting the stream, then type five letters.

```javascript
fromEvent(document, "keydown")
  .pipe(
    map(/* extract the key from the event */),
    filter(/* keep single-character keys */),
    map(/* convert the character to uppercase */),
    take(5)
  )
  .subscribe(console.log)
```

For example, typing `h`, `e`, `l`, `l`, `o` should print:

```text
H
E
L
L
O
```

### 10. Extension: describe each click

Create an Observable that prints the horizontal and vertical position of the first three clicks on the page.

A mouse event has `clientX` and `clientY` properties.

```javascript
fromEvent(document, "click")
  .pipe(
    map(/* create an object containing x and y */),
    take(3)
  )
  .subscribe(console.log)
```

Each result should have this form, although the numbers will depend on where you click:

```javascript
{ x: 250, y: 180 }
```

### Solutions

Here are some possible solutions. Other solutions may also work.

```javascript
// Exercise 1
from([1, 2, 3, 4, 5])
  .pipe(
    map(x => x * 2)
  )
  .subscribe(console.log)

// Exercise 2
from(["cat", "elephant", "mouse"])
  .pipe(
    map(word => word.length)
  )
  .subscribe(console.log)

// Exercise 3
from([1, 2, 3, 4, 5, 6])
  .pipe(
    filter(x => x % 2 === 0)
  )
  .subscribe(console.log)

// Exercise 4
from([37, 82, 49, 50, 68, 91])
  .pipe(
    filter(mark => mark >= 50)
  )
  .subscribe(console.log)

// Exercise 5
from([4, 7, 2, 9])
  .pipe(
    reduce((total, x) => total + x, 0)
  )
  .subscribe(console.log)

// Exercise 6
from([5, -2, 8, 0, 3])
  .pipe(
    filter(x => x > 0),
    map(x => x + 10),
    reduce((total, x) => total + x, 0)
  )
  .subscribe(console.log)

// Exercise 7
interval(1000)
  .pipe(
    take(5),
    map(x => x + 1)
  )
  .subscribe(seconds => console.log(`${seconds} second(s)`))

// Exercise 8
fromEvent(document, "click")
  .pipe(
    scan(count => count + 1, 0),
    take(5)
  )
  .subscribe(count => console.log(`Click number ${count}`))

// Exercise 9
fromEvent(document, "keydown")
  .pipe(
    map(event => event.key),
    filter(key => key.length === 1),
    map(key => key.toUpperCase()),
    take(5)
  )
  .subscribe(console.log)

// Exercise 10
fromEvent(document, "click")
  .pipe(
    map(event => ({
      x: event.clientX,
      y: event.clientY
    })),
    take(3)
  )
  .subscribe(console.log)
```

## Asteroids Example

As a fun, non-trivial example of how Functional Reactive Programming can help help us build complex asynchronous and interactive applications, we will work through an implementation of the classic Asteroids game.
Here is the full [Chapter](https://tgdwyer.github.io/asteroids/) from the Good Times Paradigms coursenotes that walks through the motivation behind this example.
Here is the [Finished Game](https://stackblitz.com/edit/asteroids2023).
Here is the [Workshop Code](https://stackblitz.com/edit/asteroids2023-np9nfhaa?file=src%2Fmain.ts).