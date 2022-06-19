---
layout: chapter
title: "From JavaScript to Haskell (via PureScript)"
permalink: /purescript/
---


## Learning Outcomes

* Compare a lambda-calculus inspired Haskell-like language (PureScript) with the functional programming concepts explored earlier in JavaScript
* Understand how tail call optimisation is applied in languages which support it

## Introduction

JavaScript is a multiparadigm language that—due to its support for functions as objects, closures and, therefore, higher-order functions—is able to be used in a functional programming style.  However, if you are really enamoured with currying and combining higher-order functions, then it really makes a lot of sense to use a language that is actually designed for it.

There are a number of purpose-built Functional Programming languages.  Lisp (as we have already discussed) is the original, but there are many others.  [Scheme](https://www.scheme.com/tspl4/) is a Lisp derivative, as is (more recently) [Clojure](https://clojure.org/).  SML and its derivatives (e.g. [OCaml](https://ocaml.org/), [F#](https://fsharp.org/), etc.) form another family of functional programming languages.  However, the strongest effort to build a language that holds to the principles of lambda-calculus inspired functional programming such as immutability (purity) is the Haskell family.  

There are a number of efforts to bring Haskell-like purity to web programming, inspired by the potential benefits the functional-style holds for managing complex state in asynchronous and distributed applications.  Firstly, it is possible to compile Haskell code directly to JavaScript (using [GHCJS](https://github.com/ghcjs/ghcjs)) although the generated code is opaque and requires a runtime.  Another promising and increasingly popular haskell-inspired language for client-side web development is [Elm](https://elm-lang.org/), although this again requires a runtime.  Also, Elm is rather specialised for creating interactive web apps.

The JavaScript-targeting Haskell derivative we are going to look at now is [PureScript](https://www.purescript.org/).  The reason for this choice is that PureScript generates standalone and surprisingly readable JavaScript.  For a full introduction to the language, [the PureScript Book](https://leanpub.com/purescript/read), written by the language’s creator, is available for free.  However, in this unit we will only make a brief foray into PureScript as a segue from JavaScript to Haskell.  To avoid overwhelming ourselves with minor syntactic differences we will also endeavor to stick to a subset of PureScript that is syntactically the same as Haskell.

## Hello Functional Language!

Without further ado, here is some PureScript code.  Fibonacci number computation is often called the “hello world!” of functional programming:

```haskell
fibs :: Int -> Int
fibs 0 = 1
fibs 1 = 1
fibs n = fibs (n-1) + fibs (n-2)
```

Woah!  A function for Fibonacci numbers that is about as minimal as you can get!  And the top line, which just declares the type of the function, is often optional - depending on whether the compiler can infer it from the context.  Having said that, it’s good practice to include a type declaration, especially for top-level functions (functions defined without indentation and therefore in-scope everywhere in the file).  This function takes an `Int` (integer) parameter, and returns an `Int`.  Note that the arrow shorthand for the function type definition is highly reminiscent of the JavaScript fat-arrow (`=>`) though skinnier.

The next three lines define the actual logic of the function, which very simply gives a recursive definition for the `n`th Fibonacci number.  This definition uses a feature common to many functional programming languages: *pattern matching*.  That is, we define the `fibs` function three times, with the first two definitions handling the base cases.  It says, literally: “the 0th and 1st fibs are both 1”.  The last line defines the general case, that the remaining fibonacci numbers are each the sum of their two predecessors.  Note, this definition is not perfect.  Calling:

```haskell
fibs -1
```

would be a bad idea.  Good practice would be to add some exceptions for incorrect input to our function.  In a perfect world we would have a compiler that would check types dependent on values (actually, languages that support dependent types exist, e.g. the [Idris](https://www.idris-lang.org/) language is an interesting possible successor to Haskell in this space).

One thing you will have noticed by now is that Haskell-like languages are light on syntax.  Especially, use of brackets is minimal, and typically to be avoided when evaluation order can be inferred correctly by the compiler’s application of lambda-calculus inspired precedence rules for function and operator application.

We can define a `main` function for our program, that maps the `fibs` function to a (`Nil`-terminated) linked-list of numbers and displays them to the console like so:

```haskell
main = log $ show $ map fibs $ 1..10
```
 
and here’s the output when you run it from the command line:

> (1 : 2 : 3 : 5 : 8 : 13 : 21 : 34 : 55 : 89 : Nil)

I’m omitting the type declaration for `main` because the type for functions that have input-output side-effects is a little more complicated, differs from haskell - and the compiler doesn’t strictly need it yet anyway.

The above definition for `main` is a chain of functions and the order of evaluation (and hence how you should read it) is right-to-left.  The `$` symbol is actually shorthand for brackets around everything to the symbol’s right.  In other words, the above definition for `main` is equivalent to:

```haskell
main = log ( show ( map fibs ( 1..10 )))
```

The `$` is not special syntax (i.e. it is not a keyword in the language definition).  Rather, it is an operator defined in the PureScript Prelude like so:

```haskell
infixr 0 apply as $
```

That is, `$` is an infix, right associative operator with binding precedence 0 (the lowest) that invokes the apply function:

```haskell
apply f x = f x
```

Woah!  What is f and what is `x`?  Well, in PureScript functions are generic by default - but we (and the compiler) can infer, since f x  is a function call with argument x, that f is a function and x is… anything.  So apply literally applies the function f to the argument x.  Since the binding precedence of the `$` operator is so low compared to most things that could be placed to its right, brackets are (usually) unnecessary.

---------
### Exercise

* If one didn’t happen to like the fact that function chaining with the $ operator reads right to left, how would one go about creating an operator that chains left to right?  (Hint: infixl is a thing and you will need to make a slightly different apply function also).

----------

So anyway, back to the chain of functions in `main`:

```haskell
main = log $ show $ map fibs $ 1..10
```

`log` is a function that wraps JavaScript’s console.log
`show` is a function that is overloaded to convert various types to strings.  In this case, we’ll be showing a List of Int.
`map` is (equivalent to our old friend from our JavaScript exercises) a function that applies a function to stuff inside a… let’s call it a container for now… in this case our Container is a List.
`1..10` uses the `..` (range) infix operator to create a List of Int between 1 and 10.

## Peeking under the hood

So all this may seem pretty foreign, but actually, since we’ve already covered many of the functional programming fundamentals in JavaScript, let’s take a look at the JavaScript code that the PureScript compiler generates for `fibs` and `main` and see if anything looks familiar.  Here’s `fibs`, exactly as it comes out of the compiler:

```javascript
var fibs = function (v) {
   if (v === 0) {
       return 1;
   };
   if (v === 1) {
       return 1;
   };
   return fibs(v - 1 | 0) + fibs(v - 2 | 0) | 0;
}; 
```

Woah!  It’s pretty much the way a savvy JavaScript programmer would write it.  The one part that may look a bit unusual are the expressions like `v - 1 | 0`.  Of course, JavaScript has no `Int` type, so this is PureScript trying to sensibly convert to the all-purpose JavaScript `number` type.  The `|` is a bitwise OR, so `|0` ensures that resulting expression is an integer which is both [a safety measure and a potential optimisation](https://stackoverflow.com/questions/44778826/why-does-the-purescript-compiler-generate-lots-of-0).  It's a situation where the declared types give the PureScript compiler more information about the intent of the code than would otherwise be present in JavaScript, and which it's able to use to good effect.

At first glance the code generated for `main` is a bit denser.  Here it is, again as generated by the compiler but I’ve inserted some line breaks so we can see it a little more clearly:

```javascript
var main = Control_Monad_Eff_Console.log(
  Data_Show.show(
    Data_List_Types.showList(Data_Show.showInt)
  )(
    Data_Functor.map
     (Data_List_Types.functorList)(fibs)(Data_List.range(1)(10))
  )
); 
```

Each of the functions lives in an object that encapsulates the module where it is defined.  That’s pretty standard JavaScript practice.  The rest is just function calls (application).  The call to the range function is interesting:

```javascript
Data_List.range(1)(10) 
```

Woah! It’s a curried function!  Data_List.range(1) returns a function that creates lists of numbers starting from 1.  The second call specifies the upper bound.

---------
### Exercise

* What other functions called in the JavaScript code generated for the above definition of `main` are curried?  Why?

-------------

## Tail Call Optimisation

Our definition for `fibs` was recursive.  This has a nice declarative style about it.  The definition is very close to a mathematical definition.  But at some point in your training for imperative programming you will have most likely been told that recursion is evil and inefficient.  Indeed, we’ve seen at the start of this course that there is overhead due to creating new stack frames for each function call.  Looping recursively creates a new stack frame for each iteration and so our (finite) stack memory will be consumed linearly with the number of iterations.  However, there are certain patterns of recursive function calls that our compiler can easily recognise and replace with an iterative loop.  We can see this happening directly in PureScript if we reconfigure our `fibs` definition to use a tail call.

```haskell
fibs n = f n 0 1
  where
    f 0 _ b = b
    f i a b = f (i-1) b (a+b)
```

In general, as we have seen with `$`, PureScript (and Haskell) have relatively few keywords, instead preferring functions and operators built with the language itself in the Prelude (the base library functions that are available by default).  The `where` keyword, however, is one of the exceptions.  It allows us to make some local definitions inside the scope of the function.  Here we define `f` whose first parameter is an iteration counter, whose base case is `0`.  The key feature of `f` is that its recursive call is the very last thing to happen in the function body.  That is, it is in the tail position.

The other important aspect of PureScript that we are encountering for the first time in the above definition is that indentation is used to determine scope (as in python).

Here’s the JavaScript that is generated this time:

```javascript
var fibs = function (n) {
   var f = function ($copy_v) {
       return function ($copy_v1) {
           return function ($copy_b) {
               var $tco_var_v = $copy_v;
               var $tco_var_v1 = $copy_v1;
               var $tco_done = false;
               var $tco_result;
               function $tco_loop(v, v1, b) {
                   if (v === 0) {
                       $tco_done = true;
                       return b;
                   };
                   $tco_var_v = v - 1 | 0;
                   $tco_var_v1 = b;
                   $copy_b = v1 + b | 0;
                   return;
               };
               while (!$tco_done) {
                   $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_b);
               };
               return $tco_result;
           };
       };
   };
   return f(n)(0)(1);
};
```

Obviously, it’s a less direct translation than was generated for our previous version of `fibs`.  However, you can fairly easily understand it still.  Hint, the `tco_` prefix in many of the generated variable names stands for “Tail Call Optimisation” and the local function `f` is a curried function, as are all functions of more than one argument in PureScript.  The important thing is that the recursive call is gone, replaced by a while loop.

We have seen all we need for now of PureScript.  It’s a small but nicely put together language.  It takes the best features of Haskell and reinterprets some of them quite cleverly to achieve relatively seamless interop with JavaScript.  However, it’s still a bit niche.  For the remainder of this unit [we’ll dive more deeply into Haskell](/haskell1/), which has a long history and is supported by a very large and active community across academia and industry.
