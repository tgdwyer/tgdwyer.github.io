---
layout: chapter
title: "JavaScript Introduction"
---
## Learning Outcomes

- Understand and use basic JavaScript coding concepts and features
- Understand the difference between [mutable and immutable (const) variables](#declaring-variables)
- Understand the difference between code [expressions versus statements](#expressions-versus-statements)
- Explain the relationship between javascript [functions](#functions) and [objects](#objects)
- Understand that the scope of variables is limited to [delineated code blocks](/javascript1/variable-scope) and within [functions](#functions)
- Understand that a [closure](#closures) captures variables referenced within its scope
- Create and apply [anonymous functions](#anonymous-functions) to fluent style code
- Compare [arrow functions](#arrow-functions) and regular function syntax
- Understand how JavaScripts built-in [array](#arrays) type and its [associated methods](#array-cheatsheet) can be used with arrow-functions to process data
- Explain JavaScript’s [prototype mechanism](#prototype-class-mechanism) for creating classes from functions
- Create [ES6 style classes](#ecmascript-6-class-syntax) with constructors and getters
- Compare object oriented [polymorphism](#polymorphism) to [dependency injection](#dependency-injection) through functions

## Introduction

In the late 90s the mood was right for a language that was small and simple and with executable files small enough to be distributed over the web.  Originally Java was meant to be that language but, while it quickly gained traction as a language for building general purpose applications and server-side middleware, it never really took off in the browser.  Something even simpler, and better integrated with the Document Object Model (DOM) of HTML pages was required to add basic interaction to web pages.

Brendan Eich was hired by Netscape in 1995 to integrate a Scheme interpreter into their browser for this purpose.  No messy deployment of Java bytecode bundles -- the browser would have been able to run Scheme scripts embedded directly into web pages.  This would have been awesome.  Unfortunately, for reasons that were largely political and marketing related, it was felt that something more superficially resembling Java was required.  Thus, Eich created a prototype scripting language in 2 weeks that eventually became JavaScript.  As we will see, it is syntactically familiar for Java developers.  Under the hood, however, it follows quite a different paradigm.

The fact it was initially rushed to market, the fact that browser makers seemingly had difficulty early on making standards-compliant implementations, and a couple of regrettable decisions at first regarding things like scoping semantics, meant that JavaScript developed something of a bad name.  It’s also possible that there was some inherent snobbiness amongst computer science types that, since JavaScript was not a compiled language, it must inevitably lead to armageddon.  Somehow, however, it survived and began the “web 2.0” phenomenon of what we now refer to as rich, client-side “web apps”.  It has also matured and, with the EcmaScript 6 (ES6) and up versions, has actually become quite an elegant little multi paradigm language.

The following introduction to JavaScript assumes a reasonable knowledge of programming in another imperative language such as Python or Java.

## Declaring Variables

We declare constant variables in JavaScript with the `const` keyword:

```javascript
const z = 1  // constant (immutable variable) at global scope
```

You can try this in the [debug console in a browser such as Chrome](https://developers.google.com/web/tools/chrome-devtools/console). At the console, you enter JavaScript directly at the prompt (`>`). If we try to change the value of such a `const` variable, we get a run-time error:

```javascript
> const z = 1 
> z = 2
```

> Uncaught TypeError: Assignment to constant variable.

We define mutable variables in JavaScript with the `let` keyword:

```javascript
let w = 1
```

The console replies with the value returned by the `let` statement (in Chrome console the result value is prefixed by `⋖`), as follows:

```javascript
⋖ undefined
```

But fear not, `w` was assigned the correct value which you can confirm by typing just `w` into the console:

```javascript
> w
⋖ 1
```

Now if we assign a new value to w it succeeds:

```javascript
> w = 2 // note that without a let or const keyword before it, this assignment an expression which returns a value:
⋖ 2
```

(Note: The original JavaScript syntax for declaring a variable used the `var` keyword.  However, the scoping of variables declared in this way was strange for people familiar with C and Java scoping rules, and caused much angst.  It has been fixed since ES6 with the `let` and `const` keywords; we prefer these to `var`.)

## Expressions versus Statements

We refer to any JavaScript code which evaluates to a value as an *expression*.
For example, the expression `1+1` evaluates to `2`.
We can see in the Chrome console:

```javascript
> 1 + 1
⋖ 2
```

By contrast a *statement* performs computation without returning a value. More precisely, statements evaluate to `undefined`. For example, a variable declaration is a statement:

```javascript
> const x = 1 + 1
⋖ undefined
```

Statements have to have some side effect to be useful (in this case creating a variable in the current scope).  Code blocks defined with curly braces are also statements:

```javascript
> {
    const x = 1+1
    const y = 2
    console.log(x+y)
  }
4
⋖ undefined
```

Note that `4` is printed to the console by the `console.log` (a side effect of the statement), but the value returned by the code block is `undefined`.

Unlike languages which use indentation to define code blocks and scopes (like Python and Haskell), JavaScript generally ignores multiple spaces, indentation and linebreaks.  So you can spread an expression across multiple lines and indent each line however you like:

```javascript
> 1 +
    1 + 
      1
⋖ 3
```

You can also put multiple statements on one line by separating them with '`;`':

```javascript
> const x = 1+1; const y = 2; console.log(x+y)
4
⋖ undefined
```

Of course, you should not abuse the ability to layout code in different ways and make unreadable code.  Rather, it is good practice to be consistent with formatting, use indentation to highlight nested scopes and spread things out as necessary for readability.

## JavaScript Types

JavaScript has several "primitive types" (simple types that are not [Objects](#objects)).  These include:

- `number`: any numeric value, integer or decimal
- `string`: delineated like `"hello"` or `'hello'` or even ``` `hello` ```.  
- `boolean`: can be only `true` or `false`
- `undefined`: is a special type with only one value which is `undefined`.

and a [couple of others](https://developer.mozilla.org/en-US/docs/Glossary/Primitive) that we won't worry about here.

JavaScript is *loosely typed* in the sense that a mutable variable can be assigned (and re-assigned) different values of different types.

```javascript
> let x
⋖ undefined
```

```javascript
> x = 3
⋖ 3
```

```javascript
> x = 'hello'
⋖ "hello"
```

## Variable scope

A variable's *scope* is the region of the program where it is visible, i.e. it can be referenced.
You can limit the scope of a variable by declaring it inside a block of code delineated by curly braces:

```javascript
 {
     let x = 1
     console.log(x)
 }
```

Console prints `1` from the `console.log`:

> 1

But if we try to get the value of `x`:

```javascript
x
```

> Uncaught ReferenceError: x is not defined

The above console.log statement successfully output the value of x because it was inside the same scope (the same set of curly braces).  The subsequent error occurs because we tried to look at x outside the scope of its definition.  Variables declared outside of any scope are said to be “global” and will be visible to any code loaded on the same page and could clobber or be clobbered by other global definitions -- so take care!

Be especially careful to always declare variables with either `let` or `const` keywords.  If you omit these keywords, a variable will be created at the global scope even though it is inside a `{ ... }` delimited scope, like so:

```javascript
 {
     x = 1
 }

 x
```

> 1

---

We are going to start to use a few operators that may be familiar from C or Java, some are JS specific.  
Here’s a cheatsheet:

<div class="cheatsheet" markdown="1">

## JavaScript 101: Basic Operator Cheat Sheet

### Binary Operators

```javascript
x % y   // modulo
x == y  // loose* equality
x != y  // loose* inequality
x === y // strict+ equality
x !== y // strict+ inequality

a && b   // logical and
a || b   // logical or

a & b   // bitwise and
a | b   // bitwise or
```

\* Loose (in)equality means type conversion may occur

\+ Use strict (in)equality if type is expected to be same

### Unary Operators

```javascript
i++     // post-increment
++i     // pre-increment
i--     // post-decrement
--i     // pre-decrement
!x      // not x
```

### Ternary Conditional Operator

```javascript
<condition> ? <true result> : <false result>
```

### In-place math operators

```javascript
x += <expr>
// add result of expr to x
// also -=, *=, /=, |=, &=.
```

</div>

## Functions

Functions are declared with the `function` keyword.  You can give the function a name followed by a tuple of zero or more parameters.  Variables declared within the function's body (marked by a matching pair of curly braces `{ … }`) are limited to the scope of that function body, but of course the parameters to the function are also in scope within the function body.  You return the result with the `return` keyword.

```javascript
/**
* define a function called "myFunction" with two parameters, x and y
* which does some silly math, prints something and returns the result
*/
function myFunction(x, y) {
  let t = x + y; // t is mutable
  t += z;  // += adds the result of the expression on the right to the value of t
  const result = t; // semicolons are not essential (but can help to catch errors)
  console.log("hello world"); // prints to the console
  return result; // returns the result to the caller
}

const z = 1; // z is immutable and defined in the global scope
```

You invoke (or 'call', or 'apply') a function like so:

```javascript
myFunction(1,2)
```

> hello world  
> 4

An `if-else` statement looks like so:

```javascript
/**
* get the greater of x and y
*/
function maxVal(x, y) {
   if (x >= y) {
       return x;
   } else {
       return y;
   }
}
```

This is semantically equivalent to familiar Python if/else construct, however, using `{` rather than indentation rules to establish scoping rules:

```python
def maxVal(x, y):
  if (x >= y):
    return x
  else:
    return y
```

There is also a useful ternary expression syntax for if-then-else:

```javascript
function maxVal(x, y) {
   return x >= y ? x : y;
}
```

We can loop with `while`:

```javascript
/**
* sum the numbers up to and including n
*/
function sumTo(n) {
   let sum = 0;
   while (n) { // when n is 0 this evaluates to false ending the loop
      // add n to sum then decrement n
      sum += n--; // see operator cheatsheet above
   }
   return sum;
}
sumTo(10)
```

> 55

Or `for`:

```javascript
function sumTo(n) {
   let sum = 0;
   for (let i = 1; i <= n; i++) {
       sum += i;
   }
   return sum;
}
```

This looks slightly different to a `for` loop in Python, since in Javascript we normally use 3 parts to our for loop definition (initialization, end, update)

```python
def sumTo(n):
    sum = 0
    for i in range(1, n + 1):
        sum += i
    return sum
```

Or we could perform the same computation using a recursive loop:

```javascript
function sumTo(n) {
   if (n === 0) return 0;  // base case
   return n + sumTo(n-1);  // inductive step
}
```

We can make this really succinct with a ternary if-then-else expression:

```javascript
function sumTo(n) {
   return n ? n + sumTo(n-1) : 0;
}
```

We consider this recursive loop a more “declarative” coding style than the imperative loops.

It is closer to the *inductive definition* of sum than a series of steps for how to compute it.

- No *mutable* variables used
- Each expression in this code is *“pure”*: it has no *effects* outside the expression.  Thus, you could replace each element of code with something else that produces the same result for a given input (such as a simple look up of a precomputed cache) and it would work the same.
- Therefore: this code has the property of *referential transparency*.
- The code succinctly states the *loop invariant*.

## Stack Overflow and Tail Recursion

Each time a function is invoked, the interpreter (or ultimately the CPU) will allocate another chunk of memory to a special area of memory set aside for such use called the *stack*.  The stack is finite.  A recursive function that calls itself too many times will consume all the available stack memory.

Therefore, too many levels of recursion will cause a *stack overflow*.

```javascript
sumTo(1000000)
```

> Uncaught RangeError: Maximum call stack size exceeded

However, functional languages (like Haskell) rely on recursion because they have no other way to create loops without mutable variables -- so they must have a way to make this scale to real-world computations.  When a recursive function is written in a special way, such that the recursive call is in *tail position*, [a lot of modern compilers](https://en.wikipedia.org/wiki/Tail_call#Language_support) are able to transform the recursion into a loop with constant memory use (commonly a `while` loop) -- this is called *tail call optimisation*.

Let's see what a *tail recursive* version of the `sumTo` function looks like:

```javascript
function sumTo(n, sum = 0) {
   return n ? sumTo(n-1, sum + n)
            : sum;
}
```

We have added a second parameter *sum* to store the computation as recursion proceeds.  Such parameters are called *accumulators*.  The `= 0` in the parameter definition provides a default value in case the caller does not specify an argument.  Thus, this new version can be called the same way as before:

```javascript
sumTo(10)
```

> 55

The important change is that the recursive call (on the branch of execution that requires it) is now the very last operation to be executed before the function returns.  The computation (`sum + n`) occurs before the recursive call.  Therefore, no local state needs to be stored on the stack.

Note: although it has been proposed for the EcmaScript standard, as of 2024, not all JavaScript engines support tail call optimisation (only Safari/WebKit AFAIK).  

## Functions as parameters to other functions

We can make functions more versatile by parameterising them with other functions:

```javascript
function sumTo(n, f = x => x) {
   return n ? f(n) + sumTo(n-1, f) : 0;
}
```

Note that the new parameter `f` defaults to a simple function that directly returns its argument.  Thus, called without a second parameter sumTo has the same behavior as before:

```javascript
 sumTo(10)
```

> 55

But, we can now specify a non-trivial function to be applied to the numbers before they are summed.  For example, a function to square a number:

```javascript
function square(x) {
   return x * x;
}
```

can be passed into `sumTo` to compute a sum of squares:

```javascript
sumTo(10, square)
> 385
```

## Objects

Like Java, everything in JavaScript is an object. You can construct an object populated with some data, essentially just with JSON syntax:

```javascript
const myObj = {
    aProperty: 123,
    anotherProperty: "tim was here"
};
```

However, in JavaScript, objects are simply property bags, indexed by a hashtable:

```javascript
// the following are equivalent and both involve a hashtable lookup:
console.log(myObj.aProperty);
console.log(myObj['aProperty']);
```

Note that when we declare an object with the `const` keyword as above, it is only *weakly immutable*.  This means that we cannot reassign `myObj` to refer to a different object, however, we can change the properties inside `myObj`.  Thus, the `myObj` variable is constant/immutable, but the object created by the declaration is mutable.  So, after making the above `const` declaration, if we try the following reassignment of `myObj` we receive an error:

```javascript
myObj = {
    aProperty: 0,
    anotherProperty: "tim wasn't here"
};
```

> VM48:1 Uncaught TypeError: Assignment to constant variable.

But the immutability due to `const` is *weak* or *shallow* in the sense that while the `myObj` variable which references the object is immutable, the properties of the object are mutable, i.e. we can reassign properties on `myObj` with no error:

```javascript
myObj.aProperty = 0;
```

We can also quickly declare variables that take the values of properties of an object, through *destructuring* syntax:

```javascript
const {aProperty} = myObj;
console.log(aProperty);
123
```

Which is equivalent to:

```javascript
const aProperty = myObj.aProperty;
```

This is most convenient to destructure objects passed as arguments to functions. It makes it clear from the function definition precisely which properties are going to be accessed.  Consider:

```javascript
const point = {x:123, y:456};
function showX({x}) {
   console.log(x);
}
```

You can also initialise an object's properties directly with variables.  Unless a new property name is specified, the variable names become property names, like so:

```javascript
const x = 123, tempY = 456;
const point = {x /* variable name used as property name */,
               y:tempY /* value from variable but new property name */};
point
```

> {x: 123, y: 456}

## Arrays

JavaScript has Python-like syntax for array objects:

```javascript
const a = [1,2,3];
a.length
```

> 3

```javascript
a[0]
```

> 1

```javascript
a[2]
```

> 3

You can also destructure arrays into local variables:

```javascript
const [x,y,z] = a;
z
```

> 3

Below, we see how [Anonymous Functions](#anonymous-functions) can be applied to transform arrays, and a [cheatsheet summary](#array-cheatsheet) for further functions for working with arrays.

## Dynamic Typing

The members of `myObj` are implicitly typed as `number` and `string` respectively, and as we see in the `console.log`, conversion to string happens automatically.  JavaScript is interpreted by a JavaScript engine rather than compiled into a static executable format.  Originally, this had implications on execution speed, as interpreting the program line by line at run time could be slow.  Modern JavaScript engines, however, feature Just in Time (JIT) compilation and optimisation -- and speed can sometimes be comparable to execution of C++ code that is compiled in advance to native machine code.  However, another implication remains.  JavaScript is not type checked by a compiler.  Thus, type errors cause run-time failures rather than being caught at compile time.  JavaScript is dynamically typed in that types are associated with values rather than variables.  That is, a variable that is initially bound to one type, can later be rebound to a different type, e.g.:

```javascript
let i = 123;    // a numeric literal has type number
i = 'a string'; // a string literal has type string, but no error here!
```

The C compiler would spit the dummy when trying to reassign `i` with a value of a different type, but the JavaScript interpreter is quite happy to go along with your decision to change your mind about the type of `i`.

## Functions are Objects

The nifty thing about JavaScript -- one Scheme’ish thing that presumably survived from Eich’s original plan -- is that functions are also just objects.  That is, given the following function:

```javascript
function sayHello(person) {
    console.log('hello ' + person);
}
sayHello('tim');
```

> "hello tim"

We can easily bind a function to a variable:

```javascript
const hi = sayHello;
hi('tim');
```

> "hello tim"

You can actually do the same in Python

```python
def sayHello(person):
    print('hello ' + person)
sayHello('tim')
hi = sayHello
hi('tim')
```

> "hello tim"
> "hello tim"

## Anonymous Functions

The `sayHello` function is called a *named function*.  We can also create an anonymous function to be bound immediately to a variable:

```javascript
const hi = function(person) {
    console.log("hello " + person);
};
```

or to pass as a parameter into another function. For example, `Array` objects have a `forEach` member that expects a function as an argument, which is then applied to every member of the array:

```javascript
['tim', 'sally', 'anne'].forEach(function(person) {
    console.log('hello ' + person);
})
```

> "hello tim"  
> "hello sally"  
> "hello anne"

This pattern of passing functions as parameters to other functions is now so common in JavaScript that the EcmaScript 6 standard introduced some new arrow syntax (with slightly different semantics, as explained below) for anonymous functions:

```javascript
['tim', 'sally', 'anne'].forEach(person=> console.log('hello ' + person))
```

Note that whatever value the expression on the right-hand side of the arrow evaluates to is implicitly returned. Here, we use the `map` array method to create a new array from applying the provided function to each element:

```javascript
['tim', 'sally', 'anne'].map(person=> "hello " + person)
```

> ["hello tim", "hello sally", "hello anne"]

Multiple statements (either split across lines or separated with `;`s) including local variable declarations can be enclosed in brackets with arrow syntax, but then an explicit `return` statement is required to return a value:

```javascript
['tim', 'sally', 'anne'].map(person=> {
   const message = "hello " + person;
   console.log(message);
   return message;
})
```

### Arrow Functions

As mentioned above, ES6 introduced compact notation for anonymous functions:

```javascript
const greeting = person=> 'hello' + person;
```

Which is completely equivalent to the long form:

```javascript
const greeting = function(person) {
    return 'hello ' + person;
}
```

You can also have functions with a list of arguments, just put the list in brackets as for usual function definitions:

```javascript
const greeting = (greeting, person)=> greeting + ' ' + person
```

This is equivalent to using `lambda` in Python:

```python
greeting = lambda greeting, person: greeting + ' ' + person
```

The body of the above functions are simple expressions.  If you need a more complex, multiline body (e.g. with local variables) you can do this but you need to surround the code block with curly braces `{}` and use an explicit `return` statement:

```javascript
const greeting = (greeting, person)=> {
    const msg = greeting + ' ' + person
    console.log(msg)
    return msg
};
```

### Reduce

We can use multi-parameter anonymous functions with another nifty method on `Array` objects which allows us to `reduce` them to a single value.

```javascript
[5,8,3,1,7,6,2].reduce((accumulator,x)=>accumulator+x,0)
```

> 32

The `reduce` method applies a function to each of the elements in the array in order to compute an aggregated value for the whole array.  The nature of the aggregate depends on the function you pass in.  Here we just sum the elements in the array.  The function we pass in has two parameters, the second is the array element (which we refer to here as `x`), the first parameter `accumulator` is either:

- the second argument to `reduce` (which in our case is 0), if this is the first call to the function,
- or, for every other call, the result returned by the previous call to the function.

`reduce` is incredibly versatile and can be used to do much more than sum numbers.  For example, say we want to see whether all the elements of an array pass some test.

```javascript
const all = (test, array) => array.reduce(
    (accumulator, x) => accumulator && test(x),
    true);
```

We call `test` a predicate function, i.e., a function which returns true or false. Here the `accumulator` is a boolean with initial value `true`.  If an element of the array fails the test the `accumulator` becomes `false` and stays `false`, using the `&&` operator.

```javascript
all(x => x < 5, [1, 2, 3]);
all(x => x < 5, [1, 3, 5]);
```

> true

> false

Note: Instead of writing our own `all` function, we could have used the builtin `every` array method instead:

```javascript
[1, 2, 3].every(x => x < 5);
[1, 3, 5].every(x => x < 5);
```

---

### Exercise

- Can you write a function `any` (without using the builtin `some` array method) that returns true if any of the tests pass?

- What if we wanted to see how many times each word appears in a list?

#### Solutions

`any` is similar to `reduce`, however, we use the or operator (`||`):

```javascript
const any = (test, array) => array.reduce(
    (accumulator, x) => accumulator || test(x),
    false);
```

```javascript
const wordCount = (array) => array.reduce(
    (accumulator, word) => {
        if (word in accumulator) {
            accumulator[word] += 1
        } else {
            accumulator[word] = 1
        }
        return accumulator
    },
    {}
)
```

Here the `accumulator` is an object which is initially empty.  For each word in the list the word count is either updated or created in the `accumulator` object.  Note however that this implementation is not *pure*; the aggregator function modifies `accumulator` in place before returning it.

```javascript
wordCount(['tim', 'sally', 'tim'])
```

> { tim: 2, sally: 1 }

We can modify this to be pure:

```javascript
const wordCount = (array) => array.reduce(
    (accumulator, word) => ({
        ...accumulator,
        [word]: (word in accumulator ? accumulator[word] : 0) + 1
    }),
    {}
);
```

`[word]` when used inside an object literal is an example of a computed property name. It allows you to set an object's property name based on the value of a variable.

---

<div class="cheatsheet" markdown="1">

## Array Cheatsheet

In the following, the annotations beginning with `:` describe the type of each parameter and the return type of the function.  The array `a` has elements of type `U`, and `U=>V` is the type of a function with input parameter type `U` and return type `V`.
(Note: these are not correct [TS annotations](/typescript1), but an informal “shorthand”.)

```javascript
a.forEach(f: U=> void): void  // apply the function f to each element of the array
```

Although it does not typically mutate `a`, `forEach` is impure if `f` has any side effect (which it most likely will because otherwise why would you bother!).

### Pure Methods on Array

```javascript
// Copy the whole array
a.slice(): U[]
// Copy from the specified index to the end of the array
a.slice(start: number): U[]
// Copy from start index up to (but not including) end index
a.slice(start: number, end: number): U[]

// Returns a new array with the elements of b concatenated after the
// elements of a
a.concat(b: U[]): U[]
// Returns b and c appended to a. Further arrays can be passed after c
a.concat(b: U[], c: U[]): U[]

// Apply f to elements of array and return result in new array of type
// V
a.map(f: U=> V): V[]

// Returns a new array of the elements of a for which f returns true
a.filter(f: U=> boolean): U[]
// Returns the first element of a for which f returns true, or
// undefined if there is no such element
a.find(f: U=> boolean): U | undefined
// Returns whether all elements satisfy the function f
a.every(f: U=> boolean): boolean
// Returns whether at least one element satisfies the function f
a.some(f: U=> boolean): boolean

// Uses f to combine elements of the array into a single result of
// type V
a.reduce(f: (V, U)=> V, V): V
```

All of the above are pure in the sense that they do not mutate `a`, but return the result in a new object.

*Note: the function passed to `forEach` takes an optional second parameter (not shown above) which is the index of the element being visited (see [docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach)).  While `map`, `filter`, `reduce`, `every`, & `some` have a similar optional index parameter, I suggest to avoid using it because it leads to hacky imperative style thinking about loops.*
</div>

## Closures

Functions can be nested inside other function definitions and can access variables from the enclosing scope.

**Definitions:**

- A function and the set of variables it accesses from its enclosing scope is called a *closure*.  
- Variables from the enclosing scope that are accessed by the closure are said to be *captured* by the closure.  

You can also have a function that creates and returns a closure that can be applied later:

```javascript
function add(x) {
    return y => y+x; // we are going to return a function which includes
                     // the variable x from its enclosing scope
                     // - “a closure”
}
const addNine = add(9)
addNine(10)
```

> 19

In the above example, the parameter `x` of the `add` function is captured by the anonymous function that is returned, which forms a closure.  Thus, the binding of `x` to a value *persists* beyond the scope of the `add` function itself.  Effectively, we have used the `add` function to create a new function: `y=>y+9` -- without actually writing the code ourselves.

```javascript
addNine(1)
```

> 10

We can also call the add function with two arguments at once:

```javascript
add(1)(2)
```

> 3

Functions like `add`, which operate on multiple parameters but which split the parameters across multiple nested single parameter functions, are said to be [Curried](/higherorderfunctions#curried-functions).  Compare to a traditional function of two parameters:

```javascript
function plus(x,y) { return x + y }
plus(1,2)
```

> 3

The `add` function above is a curried version of the `plus` function.  We will discuss curried functions more when we more formally introduce [higher-order functions in a later chapter](/higherorderfunctions).

Note that functions that are curried can be written in either arrow syntax or using the `function` keyword or a mix, as above.  The following versions of `add` are completely equivalent to the one above:

```javascript
function add(x) {
  return function(y) {
    return x+y
  }
}

const add = x=>y=>x+y
```

As another example, consider a curried wrapper for our `sumTo` from [before](#functions-as-parameters-to-other-functions):

```javascript
function sumOf(f) {
    return n => sumTo(n, f)
}
```

Now, we can create custom functions that compute sums over arbitrary sequences:

```javascript
const sumOfSquares = sumOf(square)
sumOfSquares(10)
```

> 385

```javascript
sumOfSquares(20)
```

> 2870

## Prototype Class Mechanism

*Note: the following way to achieve class encapsulation is deprecated by ES6 syntax -- skip to [the next section](#ecmascript-6-class-syntax) to see the modern way to do it.*

In JavaScript you can also create functions as members of objects:

```javascript
const say = {
    hello: person => console.log('hello ' + person)
}
say.hello("tim")
```

> "hello tim"

However, these objects are only single instances.  
JavaScript supports creating object instances of a certain type (i.e. having a set of archetypical members, like a Java class) through a function prototype mechanism.  You create a constructor function:

```javascript
function Person(name, surname) {
    this.name = name
    this.surname = surname
}
const author = new Person('tim', 'dwyer')
sayHello(author.name)
```

> "hello tim"

You can also add method functions to the prototype, that are then available from any objects of that type:

```javascript
Person.prototype.hello = function() { console.log("hello " + this.name) }
author.hello()
```

> "hello tim"

Note that above we use the old-style verbose JavaScript anonymous function syntax instead of the arrow form.  This is because there is a difference in the way the two different forms treat the `this` symbol.  In the arrow syntax, `this` refers to the enclosing execution context.  In the verbose syntax, `this` resolves to the object the method was called on.

It’s very tempting to use the prototype editing mechanism for evil.  For example, I’ve always wished that JS had a function to create arrays initialised over a range:

```javascript
Array.prototype.range =
  (from, to)=>Array(to)  // allocate space for an array of size `to`
  .fill()                // populate the array (with `undefined`s)
  .map((_,i)=>i)         // set each element of the array to its index
  .filter(v=> v >= from) // filter out values below from

[].range(3,9)
```

> [3,4,5,6,7,8]

Of course, if you do something like this in your JS library, and it pollutes the global namespace, and one day EcmaScript 9 introduces an actual `range` function with slightly different semantics, and someone else goes to use the `[].range` function expecting the official semantics -- well, you may lose a friend or two.

Some notes about this implementation of range:

- Although the `Array(n)` function allocates space for n elements, the result is still "empty" so `fill()` is necessary to actually create the entries.
- The function passed to `map` is using an optional second argument which receives the index of the current element.  *See note in the [Array Cheatsheat](#array-cheatsheet) suggesting not to use this*.
- The `_` is not special syntax, it's a valid variable name. `_` is a common convention for a parameter that is not used. This is seen throughout various languages such as Python, Javascript and Haskell.

---

## Exercises

- Amend the range function above to handle negative values in from or to, and add some calculation so that the array is size `to - from` from the start, eliminating the need for `filter`.
- Hack a sum function onto the `Array.prototype` (you’ll need to use an old style anonymous function to access the array through `this`).
- Why might you lose friends doing this kind of thing to built-in types?
- We are going to be dealing with linked-list like data structures a lot in this course.  Implement a linked list using JavaScript objects as simply as you can, and create some functions for working with it, like length and map.

### Solutions

```javascript
Array.prototype.range = (from, to) =>
  Array(to - from) // Correctly size the array
    .fill()
    .map((_, i) => i + from);
```

Adding a sum function on Array.prototype can be done using an old style anonymous function to access `this` which refers to the array instance:

```javascript
Array.prototype.sum = function() {
  return this.reduce((acc, val) => acc + val, 0);
};
```

Modifying built-in types, like adding functions to Array.prototype, can lead to several issues, e.g., compatibility Issues -- if future versions of JavaScript add a method with the same name but different behavior, it can break your or others' code unexpectedly, conflicts -- If different libraries try to modify the same prototype with different implementations, it can lead to conflicts that are hard to diagnose.

One possible implementation of a linked list, is storing two values in an array, the current value and the next value.

```javascript
function createNode(value, next = null) {
    return () => next ? [value, next] : [value];
}

const list = createNode(1, createNode(2, createNode(3)))

function length(list) {
    if (!list) return 0;
    const result = list();
    return 1 + (result[1] ? length(result[1]) : 0);
}

function map(f, list) {
    if (!list) return null;
    const [value, next] = list();
    return createNode(f(value), next ? map(next, f) : null);
}

```

---

## EcmaScript 6 Class Syntax

Consider another class created with a function and a method added to the prototype:

```javascript
function Person(name, occupation) {
   this.name = name
   this.occupation = occupation
}
Person.prototype.sayHello = function() {
   console.log(`Hi, my name's ${this.name} and I ${this.occupation}!`)
}
const tim = new Person("Tim","lecture Programming Paradigms")
tim.sayHello()
```

> Hi, my name's Tim and I lecture Programming Paradigms!

ES6 introduced a new syntax for classes that will be more familiar to Java programmers:

```javascript
class Person {
   constructor(name, occupation) {
       this.name = name
       this.occupation = occupation
   }
   sayHello() {
       console.log(`Hi, my name's ${this.name} and I ${this.occupation}!`)
   }
}
```

The equivalent in Java is:

```java
public class Person {
    private String name;
    private String occupation;

    public Person(String name, String occupation) {
        this.name = name;
        this.occupation = occupation;
    }

    public void sayHello() {
        System.out.println("Hi, my name's " + this.name + " and I " + this.occupation + "!");
    }
}
```

And Python:

```python
class Person:
    def __init__(self, name, occupation):
        self.name = name
        self.occupation = occupation

    def say_hello(self):
        print(f"Hi, my name's {self.name} and I {self.occupation}!")
```

There is also now syntax for “getter properties”: functions which can be invoked without `()`, i.e. to look more like properties:

```javascript
class Person {
   constructor(name, occupation) {
       this.name = name
       this.occupation = occupation
   }
   get greeting() {
       return `Hi, my name's ${this.name} and I ${this.occupation}!`
   }
   sayHello() {
       console.log(this.greeting)
   }
}
```

This is similar to the `@property` decorator you may have seen in Python:

```python
class Person:
    def __init__(self, name, occupation):
        self.name = name
        self.occupation = occupation

    @property
    def greeting():
       return f"Hi, my name's {self.name} and I {self.occupation}!"

    def say_hello(self):
        print(self.greeting)
```

And Javascript classes support single-inheritance to achieve polymorphism:

```javascript
class LoudPerson extends Person {
   sayHello() {
       console.log(this.greeting.toUpperCase())
   }
}

const tims = [
   new Person("Tim","lecture Programming Paradigms"),
   new LoudPerson("Tim","shout about Programming Paradigms")
]

tims.forEach(t => t.sayHello())
```

> Hi, my name's Tim and I lecture Programming Paradigms!  
> HI, MY NAME'S TIM AND I SHOUT ABOUT PROGRAMMING PARADIGMS!

## Polymorphism

According to Cartelli *et al.*, "Polymorphic types are types whose operations are applicable to values of more than one type."  Thus, although `Person` and `LoudPerson` are different types, since `LoudPerson` is a sub-type of `Person`, they present a common `sayHello` method allowing operations like `forEach` to operate over an array of the base class.  In a traditional Object Oriented language like Java, the compiler enforces that objects must be instances of a common base class or interface to be treated as such.  This type of polymorphism is called *subtyping polymorphism*.

In JavaScript, with no compile-time typecheck, a kind of polymorphism is possible such that if two objects both present a similarly named method that is callable in the same way, of course there is nothing preventing you simply using that method on each object as if it is the same:

```javascript
const a = {f: ()=>console.log("a")}
const b = {f: ()=>console.log("b")}
[a,b].forEach(o=>o.f())
```

> a  
> b

Informally, this type of polymorphism is called “Duck Typing” (i.e. "If it looks like a duck, swims like a duck, and quacks like a duck, then it probably is a duck").

<figure style="display: block; margin-left: auto; margin-right: auto; text-align: center;">
  <img src="/assets/images/chapterImages/javascript1/duck_typing.webp" alt="Duck Typing" style="width: 50%; display: block; margin-left: auto; margin-right: auto;" />
  <figcaption class="figure-caption" style="text-align: center;">An example of a duck, typing.</figcaption>
</figure>

Another type of polymorphism which is key to strongly typed functional programming languages (like Haskell), but also a feature of many modern OO languages is *parametric polymorphism*.  We will see this in action when we introduce [TypeScript generics](/typescript1/#generic-types).

*Reference: Cardelli, Luca, and Peter Wegner. "On understanding types, data abstraction, and polymorphism." ACM Computing Surveys (CSUR) 17.4 (1985): 471-523.*

## Dependency Injection

It’s useful to compare the above style of polymorphism to a functional approach to dependency injection:

```javascript
class Person {
   constructor(name, occupation, voiceTransform = g => g) {
       this.name = name
       this.occupation = occupation
       this.voiceTransform = voiceTransform
   }
   get greeting() {
       return `Hi, my name's ${this.name} and I ${this.occupation}!`
   }
   sayHello() {
       console.log(this.voiceTransform(this.greeting))
   }
}
const tims = [
   new Person("Tim", "lecture Programming Paradigms"),
   new Person("Tim", "shout about Programming Paradigms", g => g.toUpperCase())
]
tims.forEach(t => t.sayHello())
```

> Hi, my name's Tim and I lecture Programming Paradigms!  
> HI, MY NAME'S TIM AND I SHOUT ABOUT PROGRAMMING PARADIGMS!

So the filter property defaults to the identity function (a function which simply returns its argument), but a user of the `Person` class can inject a dependency on another function from outside the class when they construct an instance.

This is a “lighter-weight” style of code reuse or specialisation.

## Glossary

*Anonymous Function*: A function defined without a name, often used as an argument to other functions. Also known as lambda function.

*Closure*: A function and the set of variables it accesses from its enclosing scope.

*Currying*: The process of transforming a function that takes multiple arguments into a sequence of functions that each take a single argument.

*Higher-Order Function*: A function that takes other functions as arguments or returns a function as its result.

*Immutable Variable*: A variable declared with const whose value cannot be reassigned.

*Mutable Variable*: A variable declared with let that can be reassigned to different values.

*Parametric Polymorphism*: A type of polymorphism where functions or data types can be written generically so that they can handle values uniformly without depending on their type.

*Weakly Immutable*: A property of const-declared objects in JavaScript, where the variable reference is immutable, but the object's properties can still be changed.

*Pure Function*: A function that always produces the same output for the same input and has no side effects.

*Referential Transparency*: An expression that can be replaced with its value without changing the program's behavior, indicating no side effects and consistent results.

*Side Effects*: Any state change that occurs outside of a function's local environment or any observable interaction with the outside world, such as modifying a global variable, writing to a file, or printing to a console.
