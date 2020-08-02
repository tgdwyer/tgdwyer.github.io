---
layout: page
title: "JavaScript Introduction"
permalink: /javascript1/
---
## Learning Outcomes

- Understand and use basic JavaScript coding concepts and features
- Understand the difference between [mutable and immutable (const) variables](#declaring-variables)
- Explain the relationship between javascript [functions](#functions) and [objects](#objects)
- Understand function scope and the concept of a [closure](#closures)
- Create and apply [anonymous functions](#anonymous-functions) to fluent style code
- Compare [arrow functions](#arrow-functions) and regular function syntax
- Explain JavaScript’s [prototype mechanism](#prototype-class-mechanism) for creating classes from functions
- Create [ES6 style classes](#ecmascript-6-class-syntax) with constructors and getters
- Compare object oriented [polymorphism](#polymorphism) to [dependency injection](#dependency-injection) through functions

## Introduction

In the late 90s the mood was right for a language that was small and simple and with executable files small enough to be distributed over the web.  Originally Java was meant to be that language but, while it quickly gained traction as a language for building general purpose applications and server-side middleware, it never really took off in the browser.  Something even simpler, and better integrated with the Document Object Model (DOM) of HTML pages was required to add basic interaction to web pages.

Brendan Eich was hired by Netscape in 1995 to integrate a Scheme interpreter into their browser for this purpose.  No messy deployment of Java bytecode bundles - the browser would have been able to run Scheme scripts embedded directly into web pages.  This would have been awesome.  Unfortunately, for reasons that were largely political and marketing related, it was felt that something more superficially resembling Java was required.  Thus, Eich created a prototype scripting language in 2 weeks that eventually became JavaScript.  As we will see, it is syntactically familiar for Java developers.  Under the hood, however, it follows quite a different paradigm.

The fact it was initially rushed to market, the fact that browser makers seemingly had difficulty early on making standards-compliant implementations, and a couple of regrettable decisions at first regarding things like scoping semantics, meant that JavaScript developed something of a bad name.  It’s also possible that there was some inherent snobbiness amongst computer science types that, since JavaScript was not a compiled language, it must inevitably lead to armageddon.  Somehow, however, it survived and began the “web 2.0” phenomenon of what we now refer to as rich, client-side “web apps”.  It has also matured and, with the EcmaScript 6 (ES6) and up versions, has actually become quite an elegant little multi paradigm language.

The following introduction to JavaScript assumes a reasonable knowledge of programming in another imperative language such as Python or Java.

## Declaring Variables

We declare constant variables in JavaScript with the const keyword:

```javascript
const z = 1;  // constant (immutable variable) at global scope
```

You can try this in the debug console in a browser such as Chrome.  If we try to change the value of such a const variable, we get a run-time error:

```javascript
z = 2
```

> Uncaught TypeError: Assignment to constant variable.

We define mutable variables in JavaScript with the let keyword:

```javascript
let w = 1;
```

You can verify in the debugger that you are able to change the value of w.

```javascript
console.log(w)
```

> 1

```javascript
w = 2
console.log(w)
```

> 2

(Note: there is another legacy keyword for declaring variables in JavaScript “var” that has different scoping rules.  Don’t use it.)

## Variable scope

You can limit the visibility of a variable to a specific part of a program by declaring it inside a block of code delineated by curly braces:

![Observable MVC Architecture](/javascript1/console1.png)

The above console.log statement successfully output the value of x because it was inside the same scope (the same set of curly braces).  The subsequent error occurs because we tried to look at x outside the scope of its definition.  Variables declared outside of any scope are said to be “global” and will be visible to any code loaded on the same page and could clobber or be clobbered by other global definitions - so take care!

Be especially carefully to always declare variables with either let or const keywords.  If you omit these keywords, a variable will be created at the global scope, like so:

![Observable MVC Architecture](/javascript1/console2.png)

---------------------
We are going to start to use a few operators, that may be familiar from C or Java, some are JS specific.  
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

Functions are declared with the function keyword.  You can give the function a name followed by a tuple of zero or more parameters.  The scope of the function is marked by a matching pair of curly braces { … }.  You return the result with the return keyword.

```javascript
/**
* define a function called "myFunction" with two parameters, x and y
* which does some silly math, prints the value and returns the result
*/
function myFunction(x, y) {
  let t = x + y; // t is mutable
  t += z;  // += adds the result of the expression on the right to the value of t
  const result = t // semi colons are not essential (but can help to catch errors)
  console.log("hello world") // prints to the console
  return result; // returns the result to the caller
}
```

You invoke (or 'call', or 'apply') a function like so:

```javascript
myFunction(1,2)
```

> hello world  
> 4

An if statement looks like so:

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

There is also a useful ternary expression syntax for if-then-else:

```javascript
function maxVal(x, y) {
   return x >= y ? x : y;
}
```

We can loop with while:

```javascript
/**
* sum the numbers up to and including n
*/
function sumTo(n) {
   let sum = 0;
   while (n) {
       sum += n--;
   }
   return sum;
}
sumTo(10)
```

> 55

Or for:

```javascript
function sumTo(n) {
   let sum = 0;
   for (let i = 1; i <= n; i++) {
       sum += i;
   }
   return sum;
}
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
- Each expression in this code is *“pure”*: it has no *effects* outside the expression.
- Therefore: this code has the property of *referential transparency*.
- The code succinctly states the *loop invariant*.

## Stack Overflow: a caveat of recursion

Too many levels of recursion will cause a *stack overflow*.

```javascript
sumTo(1000000)
```

> Uncaught RangeError: Maximum call stack size exceeded

We can make functions more versatile by parameterising them with other functions:

```javascript
function sumTo(n, f) {
   return n ? f(n) + sumTo(n-1, f) : 0;
}

function square(x) {
   return x * x;
}
```

So we can compute the sum of the first 10 squares:

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
}
```

However, in JavaScript, objects are simply property bags, indexed by a hashtable:

```javascript
// the following are equivalent and both involve a hashtable lookup:
console.log(myObj.aProperty)
console.log(myObj['aProperty'])
```

Note that when we declare an object with the ```const``` keyword as above, it is only *weakly immutable*.  This means that we cannot reassign myObj to refer to a different object, however, we can change the properties inside myObj.  Thus, the myObj variable is constant/immutable, but the object created by the declaration is mutable.  So, after making the above const declaration, if we try the following reassignment of myObj we receive an error:

```javascript
myObj = {
    aProperty: 0,
    anotherProperty: "tim wasn't here"
}
```

> VM48:1 Uncaught TypeError: Assignment to constant variable.

But the immutability due to `const` is *shallow* in the sense that while the `myObj` variable which references the object is immutable, the properties of the object are mutable, i.e. we can reassign properties on myObj with no error:

```javascript
myObj.aProperty = 0
```

We can also quickly declare variables that take the values of properties of an object, through *destructuring* syntax:

```javascript
const {aProperty} = myObj
console.log(aProperty)
123
```

Which is equivalent to:

```javascript
const aProperty = myObj.aProperty
```

This is most convenient to destructure objects passed as arguments to functions. It makes it clear from the function definition precisely which properties are going to be accessed.  Consider:

```javascript
const point = {x:123, y:456}
function showX({x}) {
   console.log(x)
}
```

If can also initialise an object directly with variables.  Unless a new attribute name is specified, the variable names become attribute names, like so:

```javascript
const x = 123, tempY = 456
const point = {x /* variable name used as attribute name */, 
               y:tempY /* value from variable but new attribute name */}
point
```

{x: 123, y: 456}

## Arrays

JavaScript has python-like syntax for array objects:

```javascript
const a = [1,2,3]
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
const [x,y,z] = a
z
```

> 3

Below, we see how [Anonymous Functions](/javascript1#anonymous-functions) can be applied to transform arrays, and a [cheatsheet summary](/javascript1#array-cheatsheet) for further functions for working with arrays.

## Dynamic Typing

The members of myObj are implicitly typed as number and string respectively, and as we see in the console.log, conversion to string happens automatically.  JavaScript is interpreted by a JavaScript engine rather than compiled into a static executable format.  Originally, this had implications on execution speed, as interpreting the program line by line at run time could be slow.  Modern JavaScript engines, however, feature Just in Time (JIT) compilation and optimisation - and speed is becoming comparable to execution of C++ code that is compiled in advance to native machine code.  However, another implication remains.  It is not type checked by a compiler.  Thus, type errors cause run-time failures rather than being caught at compile time.  JavaScript is dynamically typed in that types are associated with values rather than variables.  That is, a variable that is initially bound to one type, can later be rebound to a different type, e.g.:

```javascript
let i = 123;    // a numeric literal has type number
i = 'a string'; // a string literal has type string, but no error here!
```

The C compiler would spit the dummy when trying to reassign i with a value of a different type, but the JavaScript interpreter is quite happy to go along with your decision to change your mind about the type of i.

## Functions are Objects

The nifty thing about JavaScript - one Scheme’ish thing that presumably survived from Eich’s original plan - is that functions are also just objects.  That is, given the following function:

```javascript
function sayHello(person) {
    console.log('hello ' + person)
}
sayHello('tim')
```

> "hello tim"

We can easily bind a function to a variable:

```javascript
const hi = sayHello
hi('tim')
```

> "hello tim"

(Note: The original JavaScript syntax for declaring a variable used the ```var``` keyword.  However, the scoping of variables declared in this way was strange for people familiar with C and Java scoping rules, and caused much angst.  It has been fixed since ES6 with the ```let``` and ```const``` keywords, we prefer these to ```var```.)

## Anonymous Functions

The sayHello function is called a *named function*.  We can also create an anonymous function to be bound immediately to a variable:

```javascript
const hi = function(person) {
    console.log("hello " + person)
}
```

or to pass as a parameter into another function, for example, Array objects have a forEach member that expects a function as an argument, which is then applied to every member of the array:

```javascript
['tim', 'sally', 'anne'].forEach(function(person) { 
    console.log('hello ' + person)
})
```

> "hello tim"  
> "hello sally"  
> "hello anne"

This pattern of passing functions as parameters to other functions is now so common in JavaScript that the EcmaScript 6 standard introduced some new arrow syntax (with slightly different semantics, as explained below) for anonymous functions:

```javascript
['tim', 'sally', 'anne'].forEach(person=> console.log('hello ' + person))
```

Note that whatever value the expression on the right-hand side of the arrow evaluates to is implicitly returned:

```javascript
['tim', 'sally', 'anne'].map(person=> "hello " + person)
```

> ["hello tim", "hello sally", "hello anne"]

Multiple ‘;’ separated statements including local variable declarations can be enclosed in brackets with arrow syntax, but then an explicit return statement is required to return a value:

```javascript
['tim', 'sally', 'anne'].map(person=> {
   const message = "hello " + person
   console.log(message)
   return message
})
```

Anonymous functions can have multiple parameters.  We see an example used with another nifty method on Array objects which allows us to `reduce` them to a single value.

```javascript
[5,8,3,1,7,6,2].reduce((accumulator,x)=>accumulator+x,0)
```

> 32

The `reduce` method applies a function to each of the elements in the array, in order to compute an aggregated value for the whole array.  The nature of the aggregate depends on the function you pass in.  Here we just sum the elements in the array.  The function we pass in has two parameters, the second is the array element (which we refer to here as `x`), the first parameter `accumulator` is either:

* the second argument to reduce(which in our case is 0), if this is the first call to the function, 
* or, for every other call, the result returned by the previous call to the function.

## Arrow Functions

As mentioned above, ES6 introduced compact notation for anonymous functions:

```javascript
const greeting = person=> 'hello' + person
```

Which is completely equivalent to the long form:

```javascript
const greeting = function(person) {
    return 'hello ' + person
}
```

You can also have functions with a list of arguments, just put the list in brackets as usual functions

```javascript
const greeting = (greeting, person)=> greeting + ' ' + person
```

The body of the above functions are simple expressions.  If you need a more complex, multiline body (e.g. with local variables) you can do this but you need to surround the code block with curly braces ```{}```:

```javascript
const greeting = (greeting, person)=> {
    const msg = greeting + ' ' + person
    console.log(msg)
    return msg
}
```

<div class="cheatsheet" markdown="1">

## Array Cheatsheet

In the following, `a` is an array with elements of type `U`, `U=>V` is the type of a function with input parameter type `U` and return type `V`
(Note: these are not correct [TS annotations](/typescript1), but a [Haskelly](/haskell1) “shorthand”)



```javascript
a.forEach(f: U=> void): void  // apply the function f to each element of the array
```
Although it does not typically mutate `a`, `forEach` is impure if `f` has any side effect (which it most likely will because otherwise why would you bother!).

### Pure Methods on Array
```javascript
a.slice(): U[]                // copy the whole array
a.slice(start: number): U[]   // copy from the specified index to the end of the array
a.slice(start: number,        // copy from start index up to 
        end: number): U[]     //    (but not including) end index
a.map(f: U=> V): V[]          // apply f to elements of array
                              //    and return result in new array of type V
a.filter(f: U=> boolean): U[] // returns a new array of the elements of a
                              //    for which f returns true
a.concat(b: U[]): U[]         // return a new array with the elements of b
                              //    concatenated after the elements of a
a.concat(b: U[], c: U[]): U[] // return b and c appended to a.
                              //    further arrays can be passed after c
a.reduce(f: (V, U)=> V, V): V // Uses f to combine elements of
                              // the array into a single result of type V
```

All of the above are pure in the sense that they do not mutate a, but return the result in a new object.

</div>

## Closures

Functions can be nested inside other function definitions and can access variables from the enclosing scope.  A function and the set of variables it accesses from its enclosing scope is called a closure.  You can also have a function that creates and returns a closure that can be applied later.

```javascript
function add(x) {
    return y => y+x; // we are going to return a function which includes
                     // the variable x from it’s enclosing scope 
                     // - “a closure”
}
let addNine = add(9)
addNine(10)
```

> 19

```javascript
addNine(1)
```

> 10

## Prototype Class Mechanism

In JavaScript you can also create functions as members of objects:

```javascript
let say = {
    hello: person => console.log('hello ' + person)
}
say.hello("tim")
```

> "hello tim"

But these objects are only single instances.  
JavaScript supports creating object instances of a certain type (i.e. having a set of archetypical members, like a Java class) through a function prototype mechanism.  You create a constructor function:

```javascript
function Person(name, surname) {
    this.name = name
    this.surname = surname
}
let author = new Person('tim', 'dwyer')
sayHello(author.name)
```

> "hello tim"

You can also add method functions to the prototype, that are then available from any objects of that type:

```javascript
Person.prototype.hello = function() { console.log("hello " + this.name) }
author.hello()
```

> "hello tim"

Note that above we use the old-style verbose JavaScript anonymous function syntax instead of the arrow form.  This is because there is a difference in the way the two different forms treat the this symbol.  In the arrow syntax, this refers to the enclosing execution context.  In the verbose syntax, this resolves to the object the method was called on.

It’s very tempting to use the prototype editing mechanism for evil.  For example, I’ve always wished that JS had a function to create arrays initialised over a range:

```javascript
Array.prototype.range = (from, to)=>Array(to).fill()
.map((_,i)=>i)
.filter(v=> v >= from)

[].range(3,9)
```

> [3,4,5,6,7,8]

Of course, if you do something like this in your JS library, and it pollutes the global namespace, and one day EcmaScript 9 introduces an actual range function with slightly different semantics, and someone else goes to use the ```[].range``` function expecting the official semantics - well, you may lose a friend or two.

---------------------

## Exercises

- Amend the range function above to handle negative values in from or to
- Hack a sum function onto the Array.prototype (you’ll need to use an old style anonymous function to access the array through this).
- Why might you lose friends doing this kind of thing to built-in types?
- We are going to be dealing with linked-list like data structures a lot in this course.  Implement a linked list using javascript objects as simply as you can, and create some functions for working with it, like length and map.

---------------------

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

There is also now syntax for “getter properties”: functions which can be invoked without (), i.e. to look more like properties:

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

And classes of course support single-inheritance to achieve polymorphism:

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

According to Cartelli and Wegner, "Polymorphic types are types whose operations are applicable to values of more than one type."  Thus, although Person and LoudPerson are different types, since LoudPerson is a sub-type of Person, they present a common sayHello method allowing operations like forEach to operate over an array of the base class.  In a traditional Object Oriented language like Java, the compiler enforces that objects must be instances of a common base class or interface to be treated as such.  This type of polymorphism is called subtyping polymorphism.

In JavaScript, with no compile-time typecheck, a kind of polymorphism is possible such that if two objects both present a similarly named method that is callable in the same way, of course there is nothing preventing you simply using that method on each object as if it is the same:

```javascript
var a = {f: ()=>console.log("a")}
var b = {f: ()=>console.log("b")}
[a,b].forEach(o=>o.f())
```

> a  
> b

Informally, this type of polymorphism is called “Duck Typing” (i.e. "If it looks like a duck, swims like a duck, and quacks like a duck, then it probably is a duck").

Another type of polymorphism which is key to strongly typed functional programming languages (like Haskell), but also a feature of many modern OO languages is parametric polymorphism.  We will see this in action when we introduce TypeScript.

## Dependency Injection

It’s useful to compare the above style of polymorphism, to a functional approach to dependency injection:

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

So the filter property defaults to the identity function (a function which simply returns its argument), but a user of the Person class can inject a dependency on another function from outside the class when they construct an instance.

This is a “lighter-weight” style of code reuse or specialisation.
