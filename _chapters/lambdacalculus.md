---
layout: page
title: "Lambda Calculus"
permalink: /lambdacalculus/
---
## Learning Outcomes

* Understand that the lambda calculus provides a complete model of computation
* Relate the lambda calculus to functional programming
* Apply conversion and reduction rules to simplify lambda expressions

## Introduction

The Lambda Calculus is a model of computation developed in the 1930s by the mathematician Alonzo Church.  You are probably aware of the more famous model for computation developed around the same time by Alan Turing: the Turing Machine.  However, while the Turing Machine is based on a hypothetical physical machine (involving tapes from which instructions are read and written) the Lambda Calculus was conceived as a set of rules and operations for function abstraction and application.  It has been proven that, as a model of computation, the Lambda Calculus is just as powerful as Turing Machines, that is, any computation that can be modelled with a Turing Machine can also be modeled with the Lambda Calculus.

The Lambda Calculus is also important to study as it is the basis of functional programming.  The operations we can apply to Lambda Calculus expressions to simplify (or reduce) them, or to prove equivalence, can also be applied to pure functions in a programming language that supports [higher-order functions](/higherorderfunctions).

Lambda Calculus expressions are written with a standard system of notation.  It is worth looking at this notation before studying haskell-like languages because it was the inspiration for Haskell syntax.  Here is a simple Lambda Abstraction of a function:
```
 λx. x
 ```
The `λ` simply denotes the start of a function expression, then follows a list of parameters (in this case we have only a single parameter called `x`) terminated by “.”  Then follows the function body, an expression returned by the function when it is applied. The variable `x` is said to be *bound* to the parameter.  Variables that appear in the function body but not in the parameter list are said to be *free*.  The above lambda expression is equivalent to the JavaScript expression: 

```javascript
x => x
```

We have already discussed combinators in JavaScript, now we can give them a more formal definition: 

> a combinator is a lambda expression (function) with no free variables.

-------
### Exercise

* When we discussed combinators in JavaScript, we gave this function a name.  What was it? *[spoiler](/higherorderfunctions#identity-i-combinator)*

-------

Some things to note about such a lambda expression:

* It has no name, it is anonymous.  Note that anonymous functions in languages like JavaScript and Python are also frequently called lambda expressions, or just lambdas.  Now you know why.
* The names of variables bound to parameters in a lambda expression are only meaningful within the context of that expression.  Thus, `λx.x` is semantically equivalent (or *alpha* equivalent) to `λy.y` or any other possible renaming of the variable.
* Lambda functions can have multiple parameters in the parameter list, e.g.: `λxy. x y`, but they are curried (e.g. a sequence of nested univariate functions) such that 

```
λxy. x y 
= λx. λy. x y 
= λx. (λy. x y)
```

What can we do with such a lambda expression?  Well we can *apply* it to another expression (The same way we can *apply* anonymous functions to an argument in JavaScript):
```
(λx. x) y
```

We can reduce this expression to a simpler form by a substitution, indicated by a bit of intermediate notation.  Two types of annotations are commonly seen, you can use either (or both!):
```
(λx. x) y      [x:=y]    -- an annotation on the right showing the substitution that will be applied to the expression on the left
(λx [x:=y].x)            -- an annotation inside the parameter list showing the substitution that will be performed inside the body (arguments have already been removed)
```
Now we perform the substitution in the body of the expression and throw away the head, since all the bound variables are substituted, leaving only:
```
y
```
This first reduction rule, substituting the arguments of a function application to all occurrences of that parameter inside the function body, is called *beta reduction*.

The next rule arises from the observation that, for some lambda term `M` that does not involve `x`:
```
λx . M  x
```
is just the same as M.  This last rule is called *eta conversion*.

----
## Lambda Calculus Cheatsheet

Three operations can be applied to lambda expressions:

**Alpha Equivalence** variables can be arbitrary renamed as long as the names remain consistent within the expression.
```
λxy.yx = λwv.vw
```

**Beta Reduction** functions are applied to their arguments by substituting the text of the argument in the body of the function
```
(λx. x) y
= (λx [x:=y]. x)
= y
```

**Eta Conversion** functions that simply apply another expression to their argument can be substituted with the expression in their body.
```
λx . M  x
= M
```
----

One thing to note about the lambda calculus is that it does not have any such thing as a global namespace.  All variables must be:

* Parameters from some enclosing lambda expression (note, below we start using labels to represent expressions - these are not variables, just placeholders for an expression that can be substituted for the label).
* Immutable - there is no way to assign a new value to a variable from within a lambda expression.

This makes the language and its evaluation very simple.  All we (or any hypothetical machine for evaluating lambda expressions) can do with a lambda is apply the three basic alpha, beta and eta reduction and conversion rules.  Here’s a fully worked example of applying the different rules to reduce an expression:


```
(λz.z) (λa.a a) (λz.z b)
⇒
(z [z:=λa.a a]) (λz.z b)      => BETA Reduction
⇒
(λa.a a) (λz.z b)
⇒
a a [a:=λz.z b]               => BETA Reduction
⇒
(λz.z b) (λz.z b)
⇒
z b [z:=(λz.z b)]             => BETA Reduction
⇒
(λz.z b) b
⇒
z b [z:=b]                    => BETA Reduction
⇒
b b
```

Note that function application is left-associative.  This means that BETA reduction is applied left to right, i.e. `( (λz.z) (λa.a a) ) (λz.z b)`.

And yet, this simple calculus is sufficient to perform computation.  We can model any of the familiar programming language constructs with lambda expressions.  For example, Booleans:

```
TRUE = λxy.x
FALSE = λxy.y
```

Note that we are using the same trick here that we used with the head and rest functions for our cons list, i.e. returning either the first or second parameter to make a choice between two options.  Now we can make an IF expression:

```
IF = λbtf.b t f
```

`IF TRUE` returns the expression passed in as `t` and `IF FALSE` returns the expression passed in as `f`.  Now we can make Boolean operators:

```
AND = λxy. IF x  y FALSE
OR = λxy. IF x TRUE y 
NOT = λx. IF x FALSE TRUE
```

These restrictions also make it a bit difficult to see how lambda calculus can be a general model for useful computation.  For example, how can we have a loop?  How can we have recursion if a lambda expression does not have any way to refer to itself?

The first hint to how loops might be possible with lambda calculus is the observation that some expressions do not simplify when beta reduced.  For example:
```
( λx . x  x) ( λy. y y)
x x [x:= y. y y] 
( λy . y  y) ( λy. y y) - which is alpha equivalent to what we started with, so goto (1).
```
Thus, the reduction would go on forever.  Such an expression is said to be divergent.  However, if a lambda function is not able to refer to itself it is still not obvious how recursion is possible.

The answer is due to the American mathematician Haskell Curry and is called the fixed-point or Y combinator:
```
 λf. ( x . f (x x) ) ( x. f (x x) )
```
-------
### Exercise

* Try using Beta reduction on the application of the Y combinator to another function (e.g. g).  A miracle occurs!

-------
If we try to directly translate the above version of the Y-combinator into JavaScript we get the following:

```javascript
const Y = f=> (x => f(x(x)))(x=> f(x(x))) // warning infinite recursion ahead!
```

Which we can then try to apply as follows:

```javascript
// A simple function that recursively calculates 'n!'.
const FAC = f => n => n>1 ? n * f(n-1) : 1
const fac = Y(FAC)
console.log(fac(6))
```
> stack overflow

And bad things happen.  Why?  JavaScript uses Eager or Strict Evaluation.  This means expressions are evaluated immediately they are encountered by the interpreter.  Let’s try doing beta reduction on the y-combinator applied to the above FAC function in lambda calculus, assuming strict evaluation:

```
( λf. (λx. f ( x x ) ) ( λx. f ( x x ) ) ) FAC
( (λx. f ( x x ) ) ( λx. f ( x x ) ) ) [ f := FAC ]
( λx. FAC ( x x ) ) ( λx. FAC ( x x ) )
FAC ( x x ) [ x := ( λx. FAC ( x x ) ) ] 
FAC ( ( λx. FAC ( x x ) ) ( λx. FAC ( x x ) ) ) ) 
FAC ( FAC ( x x ) [ x := ( λx. FAC ( x x ) ) ] )             => Beta reduction by similar steps to those above
FAC ( FAC ( FAC ( x x ) [ x := ( λx. FAC ( x x ) ) ] ) )             => and again...
FAC ( FAC ( FAC ( FAC ( x x ) [ x := ( λx. FAC ( x x ) ) ] ) ) )              => and again...
etc...
```

It just goes on forever, expanding nested expressions of FAC without actually invoking it and forcing evaluation of the expression involving n (FAC) that would otherwise cause it to terminate.  How do we restore the laziness necessary to make progress in this recursion? (hint: it involves wrapping some part of Y in another lambda.

-------
### Exercise

* Write a version of the Y-Combinator in JavaScript such that Y(Fac)(6) successfully evaluates to 120.

-------------
