---
layout: chapter
title: "Functional Reactive Programming"
---


## Introduction

This page will support the workshop solutions with a worked example of how we can use the observables, filled with pretty animations 

## Animation Generation

rxviz.com was used to create all visualization you have seen.

Here is a neat funciton allowing us to add a delay in the stream for visualization purposes.

```typescript
const addDelay = <T>(time : number) => (obs : Observable<T>) => zip(interval(time), obs).pipe(map(val => val[1]))
```

Consider, the ranks and suits, of the card, defined very similarly to the workshop
```typescript

const ranks = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K'] as const;
const suits = ['♠','♣','♦','♥'] as const;

// use typeof to define string literal types
type Rank = typeof ranks[number]; // 'A' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '10' | 'J' | 'Q' | 'K'
type Suit = typeof suits[number]; // '♠' | '♥' | '♢' | '♡'

type Card = Readonly<{
  readonly suit: Suit;
  readonly rank: Rank;
}>;

const suits$ = from(suits)
const ranks$ = from(observables)
```

Using the webtool, we can visualize each of these streams.

![Rank Observable Visualized](/assets/images/chapterImages/frpanimated/rank.gif)
![Suit Observable Visualized](/assets/images/chapterImages/frpanimated/suit.gif)


To create a card, for each suit, we can look through each rank, and create a string of suits and rank. 

```typescript
const deck = suits$.pipe(
    map(suit => rank$.pipe(
      map(rank => (`${suit}${rank}`)))))
```

![Deck Observable Visualized](/assets/images/chapterImages/frpanimated/mapDeck.gif)

However, this exists as four seperate streams. Rather, that one long continuois stream. How do we fix this? Through the `magic` of mergeMap, which can flatten the streams in to one long continous stream of cards. We now have a lovely little deck of cards :)

```typescript
const deck = suits$.pipe(
    mergeMap(suit => rank$.pipe(
      map(rank => (`${suit}${rank}`)))))
```

![Deck Observable Visualized](/assets/images/chapterImages/frpanimated/mergeMapDeck.gif)

However, this is only one deck? How can we create multiple decks. We will create a range, which will create a fixed range of numbers, and for each of those we can create a deck. 

```typescript
const decks = (numDecks : number) => range(0, numDecks).pipe(map(_ => deck))
```

![Deck Observable Visualized](/assets/images/chapterImages/frpanimated/mapDecks.gif)

This faces a similar problem, where we have multiple streams. So again, the power of mergeMap to flatten these streams in to one!

```typescript
const decks = (numDecks : number) => range(0, numDecks).pipe(mergeMap(_ => deck))
```

![Deck Observable Visualized](/assets/images/chapterImages/frpanimated/mergemapDecks.gif)


All in order, oh no, let us shuffle them. Assuming we have these functions, which can insert a element in to a random position in an array. We will use the reduce, and the randomInsertion to shuffle them.

```typescript
function impureRandomNumberGenerator(n:number) {
  return Math.floor(Math.random() * (n+1)); // impure!!
}

function randomInsert<T>(a:readonly T[],e:T): readonly T[] {
  return (i=>[...a.slice(0,i),e,...a.slice(i)])
    (impureRandomNumberGenerator(a.length + 1))
}
const shoe = (numDecks : number) => range(0, numDecks).pipe(
  mergeMap(_ => deck), 
  reduce(randomInsert, [])
)
```

This should be correct? Not quite, we `reduce` in to a single value, an array. So, now our stream contains a **single** element, an array, Rather, then being a stream of elements. This array will be all of our cards, shuffled. You can see that as we hover over the element and it attempts to print the contents.

![Deck Observable Visualized](/assets/images/chapterImages/frpanimated/singleItem.gif)

We need to turn this back in to a stream. How can we do that, with the power of mergeMap! This will take our list and convert it to a stream, and then flatten it, such that our final result is a long stream of *shuffled* cards.

```typescript
const shuffledShow = (numDecks : number) => range(0, numDecks).pipe(
  mergeMap(_ => deck), 
  reduce(randomInsert, [])
  mergeMap(from)
)
```

Wow, now we have a beautiful, shiny, shuffled deck in an Observable!

![Deck Observable Visualized](/assets/images/chapterImages/frpanimated/final.gif)
