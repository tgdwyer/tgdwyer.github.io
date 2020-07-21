---
layout: page
title: "Functional Reactive Programming"
permalink: /functionalreactiveprogramming/
---


## Learning Outcomes

## Introduction

Functional Reactive Programming describes an approach to modelling complex, asynchronous behaviours that uses many of the functional programming principles we have already explored.  In particular:

- Applications of functions to elements of containers to transform to a new container (i.e. map, filter, reduce etc. over arrays).
- Use of function composition and higher-order functions to define complex transformations from simple, reusable function elements.

We will explore FRP through an implementation of the [Observable](#observable-streams) data structure in [the Reactive Extensions for Javascript (rx.js) library](https://www.learnrxjs.io/).  We will then see it applied in application to a straight-forward [browser-based user interface problem](#a-user-interface-example).

## Observable Streams

We have seen a number of different ways of wrapping collections of things in containers: built-in JavaScript arrays, linked-list data structures, and also lazy sequences.  Now we'll see that Observable is just another type of container with some simple examples, before demonstrating that it also easily applies to asynchronous streams.  You can [also play with a live version of this code](https://stackblitz.com/edit/rxjs-introexamples?file=index.ts).

Conceptually, the Observable data structure just wraps a collection of things in a container in a similar way to each of the above.
The function ```of``` creates an Observable that will emit the specified elements in its parameter list in order.  Similar to the lazy sequences though, nothing actually happens until we initialise the stream.  We do this by "subscribing" to the Observable, passing in an "effectful" function that is applied to each of the elements in the stream.  For example, we could print the elements out with ```console.log```:

```javascript
of(1,2,3,4)
  .subscribe(console.log)
```

> 1  
> 2  
> 3  
> 4  
> 5

So, there is a similarity to the [lazy sequence](lazyevaluation) where nothing happened until we started calling ```next```, but there is also a difference.
You could think of our lazy sequences as being "pull-based" data structures, because we had to "pull" the values out one at a time by calling the ```next``` function as many times as we wanted elements of the list.  Observables are a bit different.  They are used to handle "streams" of things, such as asynchronous UI or communication events.  These things are asynchronous in the sense that we do not know when they will occur.  

Just as we have done to each of the above data structures (arrays and so on) in previous chapters, we can define a transform over an Observable to create a new Observable.  This transformation may have multiple steps the same way that we chained ```filter``` and ```map``` operations over arrays previously.  In rx.js's Observable implementation, however, they've gone a little bit more functional, by insisting that such operations are composed (rather than chained) inside a ```pipe```.  For example, here's the squares of even numbers in the range [0,10):

```javascript
range(10)
  .pipe(
    filter(x=>x%2===0),
    map(x=>x*x))
  .subscribe(console.log)
```

> 0  
> 4  
> 16  
> 36  
> 64

Now here's the solution to the first Project Euler problem, the sum of numbers divisible by 3 or 5 under 1000:

```javascript
range(1000)
  .pipe(
    filter(x=> x%3===0 || x%5===0),
    scan((a,v)=>a+v),
    last())
  .subscribe(console.log); 
```

> 233168

Scan is very much like the reduce function on Array in that it applies an accumulator function to the elements coming through the Observable, except instead of just outputting a single value (as ```reduce``` does), it emits a stream of the running accumulation (in the case the sum so far).  Thus, we use the ```last``` function to finally produce an observable with the final value.

There are also functions for combining Observable streams.  For example, ```flatMap``` gives us a way to take, for every element of a stream, a whole other stream, but flattened (or projected) together with the parent stream.  The following enumerates all the row/column indices of cells in a spreadsheet:

```javascript
const
  columns = of('A','B','C'),
  rows = range(3);

columns.pipe(
  flatMap(column=>rows.pipe(
    map(row=>[row,column])
  ))
).subscribe(([row,column])=>console.log(`Column: ${column}, Row: ${row}`))
```

> Column: A, Row: 0  
> Column: A, Row: 1  
> Column: A, Row: 2  
> Column: B, Row: 0  
> Column: B, Row: 1  
> Column: B, Row: 2  
> Column: C, Row: 0  
> Column: C, Row: 1  
> Column: C, Row: 2  

Another way to combine streams is ```merge```.  Streams that are generated with ```of``` and ```range``` have all their elements available immediately, so the result of a merge is not very interesting, just the elements of one followed by the elements of the other:

```javascript
columns.pipe(
  merge(rows)
).subscribe(console.log)
```

> A  
> B  
> C  
> 0  
> 1  
> 2  

However, as we will see in later examples ```merge``` when applied to asynchronous streams will merge the elements in the order that they arrive in the stream.

To see something more interesting we need some asynchronous streams.  For example, a stream of key- and mouse-down events from a web-page:

```javascript
const
  key$ = fromEvent<KeyboardEvent>(document,"keydown"),
  mouse$ = fromEvent<MouseEvent>(document,"mousedown");
```
It's a convention to end variable names refering to Observable streams with a ```$``` (I like to think it's short for "$tream"):

The following lets us see in the console the keys be pressed as they come in, it will keep running for as long as the web page is open:

```javascript
key$.pipe(
  map(e=>e.key)
).subscribe(console.log)
```

The following prints "Mouse Click!" on every mousedown:
```javascript
mouse$.pipe(
  map(_=>"Mouse Click!")
).subscribe(console.log)
```

Once again this will keep producing the message for every mouse click for as long as the page is open.  Note that the subscribes do not "block", so the above two subscriptions will run in parallel.  That is, we will receive messages on the console for either key or mouse downs whenever they occur.

The following achieves the same thing with a single subscription using ```merge```:
```javascript
key$.pipe(
  map(e=>e.key),
  merge(mouse$.pipe(
    map(_=>"Mouse Click!"))
  )
).subscribe(console.log)
```

## Observable Cheatsheet

The following is a very small (but sufficiently useful) subset of the functionality available for [rx.js](https://www.learnrxjs.io/).
I've simplified the types rather greatly for readability and not always included all the optional arguments.

### Creation

```typescript
// produces the list of arguments as elements of the stream
of<T>(...args: T[]): Observable<T> 

// produces a stream of numbers from 'start' until 'count' have been emitted
range(start?: number, count?: number): Observable<number>

// produces a stream for the specified event, element type depends on 
// event type and should be specified by the type parameter, e.g.: MouseEvent, KeyboardEvent
fromEvent<T>(target: FromEventTarget<T>, eventName: string): Observable<T>

// produces a stream of increasing numbers, emitted every 'period' milliseconds
interval(period?: number): Observable<number>
```

### Observable methods
```typescript
// composes together a sequence of operators (see below) that are applied to transform the stream
pipe<A>(...op1: OperatorFunction<T, A>): Observable<A>;

// {next} is a function applied to each element of the stream
// {error} is a function applied in case of error (only really applicable for communications)
// {complete} is a function applied on completion of the stream (e.g. cleanup)
// @return {Subscription} returns an object whose "unsubscribe" method may be called to cleanup
//              e.g. unsubscribe could be called to cancel an ongoing Observable
subscribe(next?: (value: T) => void, error?: (error: any) => void, complete?: () => void): Subscription;
```

### Operators

Operators are passed to ```pipe```.  They all have return type an ```OperatorFunction``` which is used by ```pipe```.

```typescript
// transform the elements of the input stream using the `project' function
map<T, R>(project: (value: T) => R)

// only take elements which satisfy the predicate
filter<T>(predicate: (value: T) => boolean)

// take `n' elements
take<T>(n: number)

// take the last element
last<T>()

// AKA concatMap: produces an Observable<R> for every input stream element<T>
flatMap<T, R>(project: (value: T) => Observable<R>)

// accumulates values from the stream
scan<T, R>(accumulator: (acc: R, value: T) => R, seed?: R)

// merge multiple Observable streams.  Actually, the resulting stream will have elements of Union type.
// i.e. the type of the elements will be the Union of the types of each of the merged streams
merge<T, R>(...observables: Observable<T>[])
```

## A User Interface Example

Modern computer systems often have to deal with asynchronous processing.  Examples abound:

- In RESTful web services, where a client sends a non-blocking request (e.g. GET) with no guarantee of when the server will send a response.
- In user interfaces, events are triggered by user interaction with different parts of the interface, which may happen at any time.
- Robotics and other systems with sensors, the system must respond to events in the world.

Under the hood, most of these systems work on an event model, a kind of single-threaded multitasking where the program (after initialisation) polls a FIFO (First-In-First-Out) queue for incoming events in the so-called event loop.  When an event is popped from the queue, any subscribed actions for the event will be applied.

In JavaScript the first event loop you are likely to encounter is the browser’s.  Every object in the DOM (Document Object Model - the tree data structure behind every webpage) has events that can be subscribed to, by passing in a callback function which implements the desired action.  We saw a basic click handler earlier.

Handling a single event in such a way is pretty straightforward.  Difficulties arise when events have to be nested to handle a (potentially-bifurcating) sequence of possible events.

A simple example that begins to show the problem is implementing a UI to allow a user to drag an object on (e.g.) an SVG canvas ([play with it here!](https://stackblitz.com/edit/frpmousedrag?file=index.ts)).  The state machine that models this is pretty simple:

![Mouse drag state machine](mousedragstatemachine.png)

There are only three transitions, each triggered by an event.  

### Turning a State-Machine into Code with Event Listeners

The typical way to add interaction in web-pages and other UIs has historically been creating the Event Listeners.  In software engineering terms it's typically referred to as the [Observer Pattern](https://en.wikipedia.org/wiki/Observer_pattern) (not to be confused with the "Observable" FRP abstraction we have been discussing).

Here’s an event-driven code fragment that provides such dragging for some SVG element ```draggableRect```, that is a child of an SVG canvas element referred to by the variable ```svg```:

```typescript
const svg = document.getElementById("svgCanvas")!;
const rect = document.getElementById("draggableRect")!;
rect.addEventListener('mousedown',e => {
    const
        xOffset = Number(rect.getAttribute('x')) - e.clientX,
        yOffset = Number(rect.getAttribute('y')) - e.clientY,
        moveListener = (e:MouseEvent)=>{
            rect.setAttribute('x',String(e.clientX + xOffset));
            rect.setAttribute('y',String(e.clientY + yOffset));
        },
        done = ()=>{
            svg.removeEventListener('mousemove', moveListener);
        };
    svg.addEventListener('mousemove', moveListener);
    svg.addEventListener('mouseup', done);
})
```

We add 'event listeners' to the html elements, which invoke the specified functions when the event fires.  There are some awkward dependencies.  The ```moveListener``` function needs access to the mouse coordinates from the mousedown event, the done function which ends the drag on a ```mouseup``` event needs a reference to the ```moveListener``` function.

It’s all a bit amorphous:

- the flow of control is not very linear or clear;
- we’re declaring callback functions inside of callback functions and the side effect of the program (that is, the change of state to the underlying web page by moving the rectangle) is hidden in the deepest nested part of the program;
- we have to manually unsubscribe from events when we’re done with them (or potentially deal with weird behaviour when unwanted zombie events fire).

The last issue is not unlike the kind of resource cleanup that [RAII](https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization) is meant to deal with.
Generally speaking, nothing about this function resembles the state machine diagram.  
The code sequencing has little sensible flow.

### The FRP Solution

We now rewrite precisely the same behaviour using Observable FRP:

```typescript
  const svg = document.getElementById("svgCanvas")!;
  const rect = document.getElementById("draggableRect")!;

  const mousedown = fromEvent<MouseEvent>(rect,'mousedown'),
        mousemove = fromEvent<MouseEvent>(svg,'mousemove'),
        mouseup = fromEvent<MouseEvent>(svg,'mouseup');

  mousedown
    .pipe(
      map(({clientX, clientY}) => ({
        mouseDownXOffset: Number(rect.getAttribute('x')) - clientX,
        mouseDownYOffset: Number(rect.getAttribute('y')) - clientY
      })),
      flatMap(({mouseDownXOffset, mouseDownYOffset}) =>
        mousemove
          .pipe(
            takeUntil(mouseup),
            map(({clientX, clientY}) => ({
                x: clientX + mouseDownXOffset,
                y: clientY + mouseDownYOffset
              })))))
   .subscribe(({x, y}) => {
     rect.setAttribute('x', String(x))
     rect.setAttribute('y', String(y))
   });
```

The Observable’s mousedown, mousemove and mouseup are like streams which we can transform with familiar operators like map and takeUntil.   The flatMap operator “flattens” the inner  mousemove  Observable stream back to the top level, then subscribe will apply a final action before doing whatever cleanup is necessary for the stream.

Compared to our state machine diagram above, we have:

- modelled each of the possible transition triggers as streams;
- the flow of data is from top to bottom, with the cycling branch introduced by the flatMap operation (which we will look into below);
- the only side effects (the movement of the rectangle) occur in the function passed to the subscribe;
- the cleanup of subscriptions to the mousemove and mouseup events is handled automatically by the ```takeUntil``` function when it closes the streams.

