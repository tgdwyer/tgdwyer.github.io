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

![Rank Observable Visualized](/assets/images/chapterImages/frpanimated/rank.png)
![Suit Observable Visualized](/assets/images/chapterImages/frpanimated/suit.png)
