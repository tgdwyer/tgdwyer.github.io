---
layout: page
title: "Observable Asteroids"
permalink: /asteroids/
---
# Introduction
Observables allow us to capture asynchronous actions like user interface events in streams.  These allow us to "linearise" the flow of control, avoid deeply nested loops, and process the stream with pure, referentially transparent functions.

As an example we will build a little "Asteroids" game using Observables.  We're going to use [rxjs](https://rxjs-dev.firebaseapp.com/) as our Observable implementation, and we are going to render it in HTML using SVG.
We're also going to take some pains to make pure functional code (and lots of beautiful curried lambda (arrow) functions). We'll use [typescript type annotations](https://www.typescriptlang.org/) to help us ensure that our data is indeed immutable and to guide us in plugging everything together without type errors.

If you're the kind of person who likes to work backwards, [you can jump straight to playing the final result](https://asteroids05.stackblitz.io/) and you can also [live edit its code](https://stackblitz.com/edit/asteroids05).

We'll build it up in several steps.
 * First, we'll just [rotate the ship](#rotating-the-ship)
  1. [with old-school events](#using-events)
  2. [with an Observable](#using-observable)
 * Then, we'll [eliminate global state using "Pure" Observable Streams](#pure-observable-streams)
 * Then, we'll [add physics and handling more inputs](#adding-physics-and-handling-more-inputs)
 * We'll [isolate the view](#view)
 * Next, we'll [introduce other objects, starting with bullets](#additional-objects)
 * Finally, we'll [deal with collisions](#collisions)

Let's start by making the svg with a simple polygon for the ship.  It will look like this:

<img width="100" src="ship.png"/>

And here's the snippet of html that creates the ship:
```html
<svg width="150" height="150" style="background-color:black">
  <g id="ship" transform="translate(75,75)">
    <polygon points="-15,20 15,20 0,-20"
                style="fill:lightblue">
    </polygon>
  </g>
</svg>
```

Note that the ship is rendered inside a transform group `<g>`.  We will be changing the `transform` attribute to move the ship around.  

# Rotating the ship
To begin with we'll make it possible for the player to rotate the ship with the arrow keys.  First, by directly adding listeners to keyboard events.  Then, by using events via Observable streams.  Here's a preview of what it's going to look like [live editor](https://stackblitz.com/edit/asteroids01):

[![Rotation animation](AsteroidsRotate.gif)](https://stackblitz.com/edit/asteroids01)

There are basically just two states, as sketched in the following state machine:

<img width="300" src="TurnStateMachine.png"/>

## Using Events
The first event we assign a function to is the window load event.  This function will not be invoked until the page is fully loaded, and therefore the SVG objects will be available.  Thus, our code begins:

```javascript
window.onload = function() {
  const ship = document.getElementById("ship")!;
  ...
```
So `ship` will reference the SVG `<g>` whose transform attribute we will be manipulating to move it.  To apply an incremental movement, such as rotating the ship by a certain angle relative to its current orientation, we will need to store that current location.  We could read it out of the transform attribute stored in the SVG, but that requires some messy string parsing.  We'll just store the state in a local object, which we will keep up to date as we move the ship.  For now, all we have is the ship's position (x and y coordinates) and rotation angle:
```javascript
...
  const state = {
      x:100, y:100, angle:0
  }
...
```
Next, we need to specify a function to be invoked on keydown events:
```typescript
...
  document.onkeydown = function(d:KeyboardEvent) {
...
```
Inside this function, we are only interested in left and right arrow keys.  If the keys are held down, after a moment they may start repeating automatically (this is OS dependent) and will churn out continuous keydown events.  We filter these out too by inspecting the KeyboardEvent.repeat property:
```typescript
...
    if((d.key === "ArrowLeft" || d.key === "ArrowRight") && !d.repeat) {
...
```
Let's say we want a left- or right-arrow keydown event to start the ship rotating, and we want to keep rotating until the key is released.  To achieve this, we use the builtin `setInterval(f,i)` function, which invokes the function `f` repeatedly with the specified interval `i` delay (in milliseconds) between each invocation.  `setInterval` returns a numeric handle which we need to store so that we can clear the interval behaviour later.
```typescript
      const handle = setInterval(function() {
        ship.setAttribute('transform',
          `translate(${state.x},${state.y}) rotate(${state.angle+=d.key === "ArrowLeft" ? -1 : 1})`)
        }, 10);
```
So as promised, this function is setting the `transform` property on the ship, using the position and angle information stored in our local `state` object.  We compute the new position by deducting or removing 1 (degree) from the angle (for a left or right rotation respectively) and simultaneously update the state object with the new angle.
Since we specify 10 milliseconds delay, the ship will rotate 100 times per second.

We're not done yet.  We have to stop the rotation on keyup by calling `clearInterval`, for the specific interval we just created on keydown (using the `handle` we stored).  To do this, we'll use `document.addEventListener` to specify a separate keyup handler for each keydown event, and since we will be creating a new keyup listener for each keydown event, we will also have to cleanup after ourselves or we'll have a memory (event) leak:
```typescript
...
      const keyupListener = function(u:KeyboardEvent) {
        if(u.key === d.key) {
          clearInterval(handle);
          document.removeEventListener('keyup',keyupListener);
        }
      };
      document.addEventListener("keyup",keyupListener);
    }
  }
}
```
And finally we're done.  But it was surprisingly messy for what should be a relatively straightforward and commonplace interaction.  Furthermore, the imperative style code above finished up deeply nested with function declarations inside function declarations, inside `if`s and variables like `d`, `handle` and `keyupListener` are referenced from inside these nested function scopes in ways that are difficult to read and make sense of.  The state machine is relatively straightforward, but it's tangled up by imperative code blocks.

## Using Observable
Observable (we'll use the implementation from rxjs) wraps common asynchronous actions like user events and intervals in streams, that we can process with a chain of 'operators' applied to the chain through a `pipe`.

We start more or less the same as before, inside a function applied on `window.onload` and we still need local variables for the ship visual and its position/angle:
```typescript
window.onload = function() {
  const 
    ship = document.getElementById("ship")!,
    state = { x:100, y:100, angle:0 };
...
```
But now we use the rxjs `fromEvent` function to create an Observable `keydown$` (the '$' is a convention indicating the variable is a stream), 
```typescript
  const keydown$ = fromEvent<KeyboardEvent>(document, 'keydown');
```
The objects coming through the stream are of type `KeyboardEvent`, meaning they have the `key` and `repeat` properties we used before.  We can create a new stream which filters these out:
```typescript
  const arrowKeys$ = keydown$.pipe(
    filter(({key})=>key === 'ArrowLeft' || key === 'ArrowRight'),
    filter(({repeat})=>!repeat));
```
To duplicate the behaviour of our event driven version we need to rotate every 10ms.  We can make a stream which fires every 10ms using `interval(10)`, which we can "graft" onto our `arrowKeys$` stream using `flatMap`.  We use `takeUntil` to terminate the interval on a `'keyup'`, filtered to ignore keys other than the one that initiated the `'keydown'`.  At the end of the `flatMap` `pipe` we use `map` to return `d`, the original keydown `KeyboardEvent` object.  Back at the top-level `pipe` on arrowKeys$ we inspect this `KeyboardEvent` object to see whether we need a left or right rotation (positive or negative angle).  Thus, `angle$` is just a stream of `-1` and `1`.
```typescript
  const angle$ = arrowKeys$.pipe(
    flatMap(d=>interval(10).pipe(
      takeUntil(fromEvent<KeyboardEvent>(document, 'keyup').pipe(
        filter(({key})=>key === d.key)
      )),
      map(_=>d))
    ),
    map(d=>d.key==='ArrowLeft'?-1:1));
```
Finally, we `subscribe` to the `angle$` stream to perform our effectful code, updating `state` and rotating `ship`.
```typescript
  angle$.subscribe(a=>
      ship.setAttribute('transform',
       `translate(${state.x},${state.y}) rotate(${state.angle+=a})`)
  )
}
```
Arguably, the Observable code has many advantages over the event handling code:
 * the streams created by `fromEvent` and `interval` automatically clean up the underlying events and interval handles when the streams complete.
 * the 'stream' abstraction provided by observable gives us an intuitive way to think about asynchronous behaviour and chain transformations of the stream together through `pipe`s.
 * We didn't see it so much here, but the various observable streams we created are composable, in the sense that adding new pipes (or potentially multiple `subscribe`s) to them allow us to reuse and plug them together in powerful ways.

# Pure Observable Streams

A weakness of the above implementation using Observable streams, is that we still have global mutable state.  Deep in the function passed to subscribe we alter the angle attribute on the `state` object.  Another Observable operator `scan`, allows us to capture this state transformation inside the stream, using a pure function to transform the state, i.e. a function that takes an input state object and---rather than altering it in-place---creates a new output state object with whatever change is required.

We'll start by altering the start of our code to define an `interface` for `State` with `readonly` members, and we'll place our `initialState` in a `const` variable that matches this interface.  You can also [play with the code in a live editor](https://stackblitz.com/edit/asteroids02).
```typescript
window.onload = function() {
  type State = Readonly<{
    x: number;
    y: number;
    angle: number;
  }>
  const initialState: State = { x: 100, y: 100, angle: 0};
```
Now we'll create a function that is a pure transformation of `State`:
```typescript
...
  function rotate(s:State, angleDelta:number): State {
    return { ...s, // copies the members of the input state for all but:
      angle: s.angle + angleDelta  // only the angle is different in the new State
    }
  }
...
```
Next, we have another, completely self contained function to update the SVG for a given state:
```typescript
...
  function updateView(state:State): void {
    const ship = document.getElementById("ship")!;
    ship.setAttribute('transform',
     `translate(${state.x},${state.y}) rotate(${state.angle})`)
  }
...
```
And now our main `pipe` (collapsed into one) ends with a `scan` which "transduces" (transforms and reduces) our state, and the `subscribe` is a trivial call to `updateView`:
```typescript
...
  fromEvent<KeyboardEvent>(document, 'keydown')
    .pipe(
      filter(({code})=>code === 'ArrowLeft' || code === 'ArrowRight'),
      filter(({repeat})=>!repeat),
      flatMap(d=>interval(10).pipe(
        takeUntil(fromEvent<KeyboardEvent>(document, 'keyup').pipe(
          filter(({code})=>code === d.code)
        )),
        map(_=>d))
      ),
      map(({code})=>code==='ArrowLeft'?-1:1),
      scan(rotate, initialState))
    .subscribe(updateView)
}
```
The code above is a bit longer than what we started with, but it's starting to lay a more extensible framework for a more complete game.  And it has some nice architectural properties, in particular we've completely decoupled our view code from our state management.  We could swap out SVG for a completely different UI by replacing the updateView function.

# Adding Physics and Handling More Inputs
Classic Asteroids is more of a space flight simulator in a weird toroidal topology than the 'static' rotation that we've provided above.  We will make our spaceship a freely floating body in space, with directional and rotational velocity.
We are going to need more inputs than just left and right arrow keys to pilot our ship too.  

Here's a sneak preview of what this next stage will look like (click on the image to try it out in a live code editor):

[![Spaceship flying](AsteroidsFly.gif)](https://stackblitz.com/edit/asteroids03?file=index.ts)

Let's start with adding "thrust" in response to up arrow.
With the code above, adding more and more intervals triggered by key down events would get increasingly messy.
Rather than have streams triggered by key events, for the purposes of simulation it makes sense that our main Observable be a stream of discrete timesteps.  Thus, we'll be moving our `interval(10)` to the top of our pipe.

Before we had just left and right rotations coming out of our stream.  Our new stream is going to have multiple types of actions as payload:
 * `Tick` - a discrete timestep in our simulation, triggered by `interval`.
 * `Rotate` - a ship rotation triggered by left or right arrows keys
 * `Thrust` - fire the boosters! Using the up-arrow key

Each of these has some data, we'll model them as simple classes:
```typescript
class Tick { constructor(public readonly elapsed:number) {} }
class Rotate { constructor(public readonly angle:number) {} }
class Thrust { constructor(public readonly on:boolean) {} }
```

Now, we'll create separate Observables for each of the key events.  There's a repetitive pattern in creating each of these Observables, for a given:
 * `Event` - keydown or keyup
 * `Key` - one of the arrows (for now)
 * produce an Observable stream of a particular action type.

It sounds like something we can model with a nice reusable function:
```typescript
  type Event = 'keydown' | 'keyup'
  type Key = 'ArrowLeft' | 'ArrowRight' | 'ArrowUp'
  const observeKey = <T>(eventName:string, k:Key, result:()=>T)=>
    fromEvent<KeyboardEvent>(document,e)
      .pipe(
        filter(({code})=>code === k),
        filter(({repeat})=>!repeat),
        map(result)),
```
Now we have all the pieces to create a whole slew of input streams:
```typescript
  const 
    startLeftRotate = observeKey('keydown','ArrowLeft',()=>new Rotate(-.1)),
    startRightRotate = observeKey('keydown','ArrowRight',()=>new Rotate(.1)),
    stopLeftRotate = observeKey('keyup','ArrowLeft',()=>new Rotate(0)),
    stopRightRotate = observeKey('keyup','ArrowRight',()=>new Rotate(0)),
    startThrust = observeKey('keydown','ArrowUp', ()=>new Thrust(true)),
    stopThrust = observeKey('keyup','ArrowUp', ()=>new Thrust(false))
```

Since we're going to start worrying about the physics of our simulation, we're going to need some helper code. First, a handy dandy Vector class.  It's just standard vector maths, so hopefully self explanatory.  In the spirit of being pure and declarative I've made it immutable, and all the functions (except `len` which returns a number) return new instances of `Vec` rather than changing its data in place. 
```typescript
class Vec {
  constructor(public readonly x: number = 0, public readonly y: number = 0) {}
  add = (b:Vec) => new Vec(this.x + b.x, this.y + b.y)
  sub = (b:Vec) => this.add(b.scale(-1))
  len = ()=> Math.sqrt(this.x*this.x + this.y*this.y)
  scale = (s:number) => new Vec(this.x*s,this.y*s)
  ortho = ()=> new Vec(this.y,-this.x)
  rotate = (deg:number) =>
            (rad =>(
                (cos,sin,{x,y})=>new Vec(x*cos - y*sin, x*sin + y*cos)
              )(Math.cos(rad), Math.sin(rad), this)
            )(Math.PI * deg / 180)

  static unitVecInDirection = (deg: number) => new Vec(0,-1).rotate(deg)
  static Zero = new Vec();
}
```
To implement the toroidal topology of space, we'll need to know the canvas size. 
For now, we'll hard code it in a constant `CanvasSize`.  Alternately, we could query it from the svg element, or we could set the SVG size - maybe later.
The torus wrapping function will use the `CanvasSize` to determine the bounds and simply teleport any `Vec` which goes out of bounds to the opposite side.
```typescript
const 
  CanvasSize = 200,
  torusWrap = ({x,y}:Vec) => { 
    const wrap = (v:number) => 
      v < 0 ? v + CanvasSize : v > CanvasSize ? v - CanvasSize : v;
    return new Vec(wrap(x),wrap(y))
  };
```
We'll use `Vec` in a slightly richer set of State.
```typescript
  type State = Readonly<{
    pos:Vec, 
    vel:Vec,
    thrust:boolean,
    angle:number,
    rotation:number,
    torque:number
  }>
```
We create an `initialState` using `CanvasSize` to start the spaceship at the centre:
```typescript
  const initialState:State = {
      pos: new Vec(CanvasSize/2,CanvasSize/2), 
      vel: Vec.Zero, 
      thrust: false, 
      angle:0,
      rotation:0,
      torque:0
  }
```
We can encapsulate all the possible transformations of state in a function:
```typescript
  const reduceState = (s:State, e:Rotate|Thrust|Tick)=>
    e instanceof Rotate ? {...s,
      torque:e.angle
    } :
    e instanceof Thrust ? {...s,
      thrust:e.on
    } : {...s,
      rotation: s.rotation+s.torque,
      angle:s.angle+s.rotation,
      pos: torusWrap(s.pos.sub(s.vel)),
      vel:s.thrust?s.vel.sub(Vec.unitVecInDirection(s.angle).scale(0.05)):s.vel
    };
```
And finally we `merge` our different inputs and scan over `State`, and the final `subscribe` calls the `updateView`, once again, a self-contained function which does whatever is required to render the State.  We describe the updated `updateView` in the next section.

```typescript
  interval(10)
    .pipe(
      map(elapsed=>new Tick(elapsed)),
      merge(
        startLeftRotate,startRightRotate,stopLeftRotate,stopRightRotate),
      merge(startThrust,stopThrust),
      scan(reduceState, initialState))
    .subscribe(updateView);
```
# View
Once again, the above completely decouples the view from state management.  But now we have a richer state, we have more stuff we can show in the view.  We'll start with a little CSS, not only to style elements, but also to hide or show flame from our boosters.
```css
.ship {
  fill: lightblue;
}
.booster {
  fill: orange;
}
.hidden {
  visibility:hidden;
}
```
We add a few more polygons for the booster flames:
```html
<script src="/bundle.js"></script>
<svg id="svgCanvas" width="200" height="200" style="background-color:black">
  <g id="ship" transform="translate(100,100)">
    <polygon class="booster hidden" id="forwardThrust" points="-3,20 0,35 3,20">
    </polygon>
    <polygon class="booster hidden" id="leftThrust" points="2,-10 15,-12 2,-14">
    </polygon>
    <polygon class="booster hidden" id="rightThrust" points="-2,-10 -15,-12 -2,-14">
    </polygon>
    <polygon class="ship" points="-15,20 15,20 0,-20">
    </polygon>
  </g>
</svg>
```
And here's our updated updateView function where we not only move the ship but also show flames shooting out of it as it powers around the torus:
```typescript
  function updateView(s: State) {
    const 
      ship = document.getElementById("ship")!,
      show = (id:string,condition:boolean)=>((e:HTMLElement) => 
        condition ? e.classList.remove('hidden')
                  : e.classList.add('hidden'))(document.getElementById(id)!),
    show("leftThrust",  s.ship.torque<0);
    show("rightThrust", s.ship.torque>0);
    show("thruster",    s.ship.acc.len()>0);
    ship.setAttribute('transform', `translate(${s.pos.x},${s.pos.y}) rotate(${s.angle})`);
  }
```

# Additional Objects
Things get more complicated when we start adding more objects to the canvas that all participate in the physics simulation.  Furthermore, objects like asteroids and bullets will need to be added and removed from the canvas dynamically - unlike the ship whose visual is currently defined in the `svg` and never leaves.  We'll start with bullets that can be fired with the Space key, and which expire after a set period of time:

[![Spaceship flying](AsteroidsShoot.gif)](https://stackblitz.com/edit/asteroids04?file=index.ts)

However, the basic framework above is a good basis on which to extend.  The first complication is generalising bodies that participate in the force model with their own type `Body`, separate from the `State`:
```typescript
  type Body = Readonly<{
    id:string,
    pos:Vec, 
    vel:Vec,
    thrust:boolean,
    angle:number,
    rotation:number,
    torque:number,
    radius:number,
    createTime:number
  }>
  type State = Readonly<{
    time:number,
    ship:Body,
    bullets:ReadonlyArray<Body>,
    rocks:ReadonlyArray<Body>,
    exit:ReadonlyArray<Body>,
    objCount:number
  }>
```
So the `ship` is a `Body`, and we will have collections of `Body` for both `bullets` and `rocks`.  What's this `exit` thing?  Well, when we remove something from the canvas, e.g. a bullet, we'll create a new state with a copy of the `bullets` array minus the removed bullet, and we'll place that removed bullet - together with other removed `Body`s to the `exit` array.  This notifies the `updateView` function that they can be removed.

Note the `objCount`.  This counter is incremented every time we add a `Body` and gives us a way to create a unique id that can be used to match the `Body` against its corresponding view object.

Now we define functions to create objects:
```typescript
  function createBullet(s:State):Body {
    const d = Vec.unitVecInDirection(s.ship.angle);
    return {
      id: `bullet${s.objCount}`,
      pos:s.ship.pos.add(d.scale(20)),
      vel:s.ship.vel.add(d.scale(-2)),
      createTime:s.time,
      thrust:false,
      angle:0,
      rotation:0,
      torque:0,
      radius:3
    }
  }
  function createShip():Body {
    return {
      id: 'ship',
      pos: new Vec(CanvasSize/2,CanvasSize/2),
      vel: Vec.Zero,
      thrust:false,
      angle:0,
      rotation:0,
      torque:0,
      radius:20,
      createTime:0
    }
  }
  const initialState:State = {
    time:0,
    ship: createShip(),
    bullets: [],
    exit: [],
    objCount: 0
  }
```
We'll add a new action type and observable for shooting with the space bar:
```typescript
  class Shoot { constructor() {} }
  const shoot = keyObservable('keydown','Space', ()=>new Shoot())
```
And now a function to move objects, same logic as before but now applicable to any `Body`:
```typescript
  const moveObj = (o:Body) => <Body>{
    ...o,
    rotation: o.rotation + o.torque,
    angle:o.angle+o.rotation,
    pos:torusWrap(o.pos.sub(o.vel)),
    vel:o.thrust?o.vel.sub(Vec.unitVecInDirection(o.angle).scale(0.05)):o.vel
  }
```
And our tick action is a little more complicated now, complicated enough to warrant its own function:
```typescript
  const tick = (s:State,elapsed:number) => {
    const not = <T>(f:(x:T)=>boolean)=>(x:T)=>!f(x),
      expired = (b:Body)=>(elapsed - b.createTime) > 100,
      expiredBullets:Body[] = s.bullets.filter(expired),
      activeBullets = s.bullets.filter(not(expired));
    return <State>{...s, 
      ship:moveObj(s.ship), 
      bullets:activeBullets.map(moveObj), 
      exit:expiredBullets,
      time:elapsed
    }
  }
```
Note that bullets have a life time (presumably they are energy balls that fizzle into space after a certain time).  When a bullet expires it is sent to `exit`.

Now adding bullets as they are fired to our state reducer:
```typescript
  const reduceState = (s:State, e:Rotate|Thrust|Tick|Shoot)=>
    e instanceof Rotate ? {...s,
      ship: {...s.ship,torque:e.direction}
    } :
    e instanceof Thrust ? {...s,
      ship: {...s.ship, thrust:e.on}
    } :
    e instanceof Shoot ? {...s,
      bullets: s.bullets.concat([createBullet(s)]),
      objCount: s.objCount + 1
    } : 
    tick(s,e.elapsed);
```
We merge the Shoot stream in as before:
```typescript
  interval(10).pipe(
...
    merge(shoot),
...
```
And we tack a bit on to `updateView` to draw and remove bullets:
```typescript
  function updateView(s: State) {
...
    s.bullets.forEach(b=>{
      const createBulletView = ()=>{
        const v = document.createElementNS(svg.namespaceURI, "ellipse")!;
        v.setAttribute("id",b.id);
        v.classList.add("bullet")
        svg.appendChild(v)
        return v;
      }
      const v = document.getElementById(b.id) || createBulletView();
      v.setAttribute("cx",String(b.pos.x))
      v.setAttribute("cy",String(b.pos.y))
    })
    s.exit.forEach(o=>{
      const v = document.getElementById(o.id);
      if(v) svg.removeChild(v)
    })
  }
```
# Collisions
So far the game we have built allows you to hoon around in a space-ship blasting the void with fireballs which is kind of fun, but not very challenging.  The Asteroids game doesn't really become "Asteroids" until you actually have... asteroids.  Also, you should be able to break them up with your blaster and crashing into them should end the game.  Here's a preview:

[![Spaceship flying](AsteroidsComplete.gif)](https://stackblitz.com/edit/asteroids05?file=index.ts)

Before we go forward, let's put all the magic numbers that are starting to permeate our code in one, immutable place:
```typescript
const 
  Constants = new class {
    readonly CanvasSize = 600;
    readonly BulletExpirationTime = 1000;
    readonly BulletRadius = 3;
    readonly BulletVelocity = 2;
    readonly StartRockRadius = 30;
    readonly StartRocksCount = 5;
    readonly RotationAcc = 0.1;
    readonly ThrustAcc = 0.1;
    readonly StartTime = 0;
  }
```

## Initial State
We will need to store two new pieces of state: the collection of asteroids (`rocks`) which is another array of `Body`, just like bullets; and also a boolean that will become `true` when the game ends due to collision between the ship and a rock. 
```typescript
  type State = Readonly<{
    ...
    rocks:ReadonlyArray<Body>,
    gameOver:boolean
  }>
```
Since bullets and rocks are both just circular `Body`s with constant velocity, we can generalise what was previously the `createBullet` function to create either:
```typescript
  const createCircle = (viewType: ViewType)=> (oid:number)=> (time:number)=> (radius:number)=> (pos:Vec)=> (vel:Vec)=>
    <Body>{
      createTime: time,
      pos:pos,
      vel:vel,
      acc:Vec.Zero,
      angle:0, rotation:0, torque:0,
      radius: radius,
      id: viewType+oid,
      viewType: viewType
    };
```

Our initial state is going to include several rocks drifting in random directions, as follows:
```typescript
  const
    startRocks = [...Array(Constants.StartRocksCount)]
      .map((_,i)=>createCircle("rock")(i)
         (Constants.StartTime)(Constants.StartRockRadius)(Vec.Zero)
         (new Vec(0.5 - Math.random(), 0.5 - Math.random()))),
    initialState:State = {
      time:0,
      ship: createShip(),
      bullets: [],
      rocks: startRocks,
      exit: [],
      objCount: Constants.StartRocksCount,
      gameOver: false
    }
```

## Reducing State

Our `tick` function is more or less the same as above, but it will apply one more transformation to the state that it returns, by applying the following function.  This function checks for collisions between the ship and rocks, and also between bullets and rocks. 

```typescript```
  handleCollisions = (s:State) => {
    const
      bodiesCollided = (a:Body,b:Body) => a.pos.sub(b.pos).len() < a.radius + b.radius,
      shipCollided = s.rocks.filter(r=>bodiesCollided(s.ship,r)).length > 0,
      allBulletsAndRocks = flatMap(s.bullets,b=>s.rocks.map(r=>({bullet:b,rock:r}))),
      collidedBulletsAndRocks = allBulletsAndRocks.filter(({bullet:b,rock:r})=>bodiesCollided(b,r)),
      collidedBullets = collidedBulletsAndRocks.map(({bullet})=>bullet),
      collidedRocks = collidedBulletsAndRocks.map(({rock})=>rock),
      createChildRock = (r:Body,dir:number)=>
        createCircle('rock')(0/*we assign the ids later*/)
                  (s.time)(r.radius/2)
                  (r.pos)(r.vel.ortho().scale(dir)),
      spawnChildRocks = (r:Body)=>
                            r.radius >= Constants.StartRockRadius/4 
                            ? [createChildRock(r,1),createChildRock(r,-1)] : [],
      newRocks = flatMap(collidedRocks, spawnChildRocks)
        .map((r,i)=><Body>{...r, id: r.viewType + (s.objCount + i)})
    return <State>{
      ...s,
      bullets: s.bullets.filter(b=>!collidedBullets.includes(b)),
      rocks: s.rocks.filter(r=>!collidedRocks.includes(r)).concat(newRocks),
      exit: s.exit.concat(collidedBullets,collidedRocks),
      objCount: s.objCount + newRocks.length,
      gameOver: shipCollided
    }
  };
```

## View

Finally, we need to update `updateView` function.  First, we need to update the visuals for each of the rocks, but these are the same as bullets.  The second, slightly bigger, change, is simply to display the text "Game Over" on `s.gameover` true. 
```typescript
  function updateView(s: State) {
  ...
    s.bullets.forEach(updateBodyView);
    s.rocks.forEach(updateBodyView);
    s.exit.forEach(o=>{
      const v = document.getElementById(o.id);
      if(v) svg.removeChild(v);
    })
    if(s.gameOver) {
      subscription.unsubscribe();
      const v = document.createElementNS(svg.namespaceURI, "text")!;
      attr(v,{x:Constants.CanvasSize/6,y:Constants.CanvasSize/2,class:"gameover"});
      v.textContent = "Game Over";
      svg.appendChild(v);
    }
  }
```
where we've created a little helper function `attr` to bulk set properties on an `Element`:
```typescript
  const
    attr = (e:Element,o:any) =>
      { for(const k in o) e.setAttribute(k,String(o[k])) },
```
The other thing happening at game over, is the call to `subscription.unsubscribe`.  This `subscription` is the object returned by the subscribe call on our main Observable:
```typescript
  const subscription = interval(10).pipe(
    map(elapsed=>new Tick(elapsed)),
    merge(
      startLeftRotate,startRightRotate,stopLeftRotate,stopRightRotate),
    merge(startThrust,stopThrust),
    merge(shoot),
    scan(reduceState, initialState)
    ).subscribe(updateView);
```
At this point we have more-or-less all the elements of a game.  The implementation above could be extended quite a lot.  For example, we could add score, multiple lives, perhaps some more physics.  But generally, these are just extensions to the framework above: manipulation and then display of additional state.

The key thing is that the observable has allowed us to keep well separated state management (model), its input and manipulation (control) and the visuals (view).  Further extensions are just additions within each of these elements - and doing so should not add greatly to the complexity.

I invite you to click through on the animations above, to the live code editor where you can extend or refine the framework I've started. 