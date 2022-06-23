---
layout: chapter
title: "FIT2102 Assignment 2: Gin Rummy"
---
# FIT2102 Assignment 2: Gin Rummy

 - **Due Date**: November 8^th^, 23:55
 - **Weighting**: 30% of your final mark for the unit
 - **Uploader**: <https://fit2102.monash/uploader/>
 - **Overview**: Your goal is to implement a player for the game of Gin Rummy.
   Your player needs to be able to play a valid game; manage a "memory" string
   with a parser-combinator; and, leverage concepts from the course. You will
   also need to write a two-page-report describing your submission.
 - **Building and using the code**: The code bundle is packaged the same way as
   tutorials. To compile the code, run: `stack build`. To execute the code,
   run: `stack exec staticgame`. If you want to play with more players, you will
   need to edit `staticgame/Main.hs`. You cannot edit the `stack` configuration.
 - **Submission**: Your player source code and your report in PDF format go in
   the `submission/` folder of the code bundle. To submit you will zip up *just
   the contents of this `submission/` folder* into one file named
   `studentNo_name.zip`.

## Gin Rummy

Gin Rummy is a two-player game where the objective is to score 100 points before
your opponent. Points are awarded to the player whose hand has the lowest value
at the end of a round. The value of a hand depends on how you decide to organise
your cards -- called "forming melds."

Your task for this assignment is to implement a player able to play a
non-trivial game of Gin Rummy. We will use a classic deck of 52 cards, aces
low.[^6]

You can find a longer explanation
[online](https://www.coololdgames.com/card-games/rummy/gin/), or even [play the
game](https://cardgames.io/ginrummy/). Do note, though, that the variant we use
has a few key differences, namely (compared to the linked resources):

 - The non-dealer does not get to *take the up-card,* the game starts after
   dealing.
 - There is non *laying-off*, we only count the melds formed in your own hand.
 - You cannot discard the card you just drew.
 - There is no *Big Gin,* you always have to discard at the end of your turn.
 - If no player takes an action before the stock runs out, the last player to
   draw is considered to have Knocked.
 - You cannot *call* (Gin or Knock) during the first turn.
 - There is a maximum of 200 turns (100 actions / player) per round, this is to
   avoid a case where neither player draws from the stock.

In the variant of Gin Rummy we use, a round (also called "playing a hand")
proceeds as follows:

 1. A *dealer* is chosen at random; this player will be last to go.
 2. Each player is dealt *ten cards;* these form *a hand*.
 3. The first card after dealing two hands is revealed and put face-side up,
    this forms the *discard*; the rest of the cards form the *stock*.
 4. In turn, each player then decides whether they want to pick the (visible)
    card from the discard, or the (hidden) top card from the stock.
 5. To end their turn, players will have to discard a card from their hand and
    announce if they want to end the game.
    
At the end of their turn, players thus discard a card and have three choices:

 1. *Call Gin*, which means that they managed to fit all ten cards in their hand
    into meld. Calling Gin awards bonus points.
 2. *Knock,* which means that, although they did not manage to fit all ten cards
    into melds, they believe to have a hand of lower value that their
    opponent's. You can only Knock if your deadwood's total value is less than 10.
 3. *Discard,* which means that they do not want to end the game.
 
### Forming melds

The core mechanic of Gin Rummy is to fit cards into melds -- think poker
combinations. In our variant of Gin Rummy, we will use three types of melds:[^5]

  - **Straight:** a combination of three to five cards of the same suit with
    consecutive numbers. For example: **A♠ 2♠ 3♠**.
  - **Set:** a combination of three or four cards with the same rank in different
    suits. For example: **<span style="color: red">8♥ 8♦</span> 8♠**.
  - **Deadwood:** any card which does not fit into a meld.
  
Now, the interesting part of Gin Rummy is that melds are not cumulative. This
means that a hand of cards can form different melds. Consider the following
cards: **7♣ 7♠ <span style="color: red">7♦ 8♦ 9♦</span>**, the seven of diamonds
can be included in either a straight or a set but cannot be included in both.
The strategy in this game is thus to decide which cards should belong to which
melds.

### Counting points

The other important part of Gin Rummy is how to count points. There are two
things that matter for scoring: the number of points (or value) of a hand and
the bonus points awarded at the end of the game.

The winner of a round is the player who finishes with the lowest value hand.
Cards that fit into melds are worth 0 points, while deadwood is counted
according to rank: face cards are worth 10 points, aces 1 points and other cards
according to their numeric value.

The winner of a round will score the difference in value between theirs and
their opponent's total deadwood value. If the winner called Gin, they will be
awarded 25 bonus points. In case a player Knocked but has a higher deadwood
count than their opponent, their opponent wins and gets awarded 10 bonus points.

The game stops when a player reaches 100 points, he is declared the winner.

## Deliverable

For this assignment, you are required to implement a player exposing three
functions. These functions are:

 1. `actionFunc`, called at the start of your turn where you choose which pile
    to draw from.
 2. `playFunc`, called after you drew a card and where you decide what to
    announce.
 3. `meldFunc`, called at the end of the round where you return the melds formed
    with your last hand.

A skeleton for the file can be found in `submission/Player.hs` in the
code bundle.

To keep the playing field level, and to allow us to evaluate your code, we ask
you use only the libraries provided. In short, you cannot edit the `stack.yaml`
and `package.yaml` or add functionality to the source code (in `src/`).

You will need to submit a file called `studentNo_name.zip` which you will create
by zipping the contents of the `submission/` directory. 

If you have any extension, you will need to include them in a directory titled
`extensions/` in your zip file. If your extension requires additional library,
feel free to include your whole project -- except build files, e.g.
`.stack-work/` -- in the `extensions/` folder.

### Choosing an action

At the beginning of its turn, your player will need to decide whether to draw a
card from the discard or the stock. The first parameter of the function is the
card on top of the discard. The last parameter is your player's current hand.

This function will also receive extra information about the state of the game.
This information is:

- `(Score, Score)` the score as of last round as: *(your score, opponent
  score)*. In the first round of a game, this will be `(0, 0)`.
- `Maybe String` what was your last memory. In the first turn of the first
  round, this will be `Nothing`. In subsequent turns, you will receive the
  memory you returned from `playCard`; the game does not change your memory in
  any way. In the first turn of each subsequent round, you will receive the last
  memory you returned in the previous round. This will allow you to keep track
  of parameters during a whole game.
- `Maybe Draw` what action you opponent took if they played before you. If you
  are the dealer, this will be `Nothing` in the first round.

``` haskell
data Draw = Stock | Discard

-- | Action function type.
--
-- This function is called at the beginning of a turn before the player has to
-- form melds.
type ActionFunc
  = Card            -- ^ card on top of the discard pile
  -> (Score, Score) -- ^ scores of (player, opponent) as of last round
  -> Maybe String
  -- ^ player's memory, on first player turn in the first round it will be Nothing
  -> Maybe Draw -- ^ opponent's chosen action, on first game turn it will be Nothing
  -> [Card]     -- ^ the player's hand
  -> (Draw, String) -- ^ which pile did the player chose to draw from and memory
```

### Managing the hand

After having chosen where to draw a card from, your player will be called again
with the drawn card. It will need to decide which card to discard and what to
announce. 

The first argument, `Card`, is the card your player drew, it is not added to
your hand directly. The last argument is your player's hand. Then, we have
similar parameters to `pickCard`:

- `(Score, Score)` the score as of the previous round as: *(your score, opponent
  score)*.
- `String` the memory you returned from `pickCard` (see above).

```haskell
data Action = Action Act Card
data Act = Gin | Knock | Drop

-- | Play function type.
--
-- A player receives the card he decided to draw (from discard or stock), her
-- hand and her memory. She then choses whether to Knock or Discard.
type PlayFunc
  = Card              -- ^ picked card
  -> (Score, Score)   -- ^ scores of (player, opponent) as of last round
  -> String           -- ^ the player's memory
  -> [Card]           -- ^ the player's hand (without new card)
  -> (Action, String) -- ^ the player's chosen card and new memory
```

### Forming melds

Finally, your player needs to be able to convert a hand of cards into melds to
do the scoring. Your melds will be checked against the rules, obviously.

This function also receives additional information about the game. Though this
may not be strictly necessary,[^7] it makes the game more consistent. (For
example, some might prefer storing their melds in memory.)

One key difference is that you cannot modify your memory in this function. This
is because `makeMeld` is called *after* a round finishes, thus we cannot form a
`Play` with its return.

``` haskell
data Meld =
      Deadwood Card            -- An unmelded card
    | Set3 Card Card Card      -- 3 cards of same rank different suit
    | Set4 Card Card Card Card -- 4 cards of same rank different suit
    | Straight3 Card Card Card -- 3 cards of same suit, sequential ranks
    | Straight4 Card Card Card Card -- 4 cards of same suit, sequential ranks
    | Straight5 Card Card Card Card Card -- 5 cards of same suit, sequential ranks

-- | Meld function type.
--
-- Which melds to use for scoring.
type MeldFunc
  = (Score, Score) -- ^ scores of (player, opponent) as of last round
  -> String        -- ^ the player's memory
  -> [Card]        -- ^ cards in player's hand
  -> [Meld]        -- ^ elected melds
```

### Managing the memory

The common component of all the functions above is the `String` which is your
player's *memory*. Your player needs to be able to keep track of some parameters
of the game through time. This is enabled by returning a `String` object after
playing.

Internally, your player should use a custom datatype to store information rather
than a `String`. To enable conversion to and from your datatype, you will have
to use a parser-combinator as presented in the course notes. The source code is
included in `src/Parser/`.

Another thing that can be considered as *memory* is the score. At each of your
function calls, you will be given the score of the *last round* as: `(your
score, opponent score)`. This can help you adjust your strategy. 

Below is an example of different values `pickCard` can receive:

| Game  | Round | Player | Score  | Memory in | Draw               | Memory out |
|-------|-------|--------|--------|-----------|--------------------|------------|
| First | First | A      | (0, 0) | Nothing   | Nothing            | "a"        |
|       |       | B      | (0, 0) | Nothing   | Just Stock/Discard | "b"        |
|       | Next  | A      | (0, 0) | Just "a"  | Just Stock/Discard | "a"        |
|       |       | B      | (0, 0) | Just "b"  | Just Stock/Discard | "b"        |
| Next  | First | B      | (n, m) | Just "b"  | Nothing            | "d"        |
|       |       | A      | (m, n) | Just "a"  | Just Stock/Discard | "c"        |
|       | Next  | B      | (n, m) | Just "d"  | Just Stock/Discard | "d"        |
|       |       | A      | (m, n) | Just "c"  | Just Stock/Discard | "c"        |

*Note*: Your memory cannot exceed 10,000 characters.

## Assessment

The assessment for this assignment will be in four parts:

 1. **Report (25%)**
 2. **Code quality (30%)**
 3. **Memory and parsing (25%)**
 4. **Player (20%)**

### Report

You are required to provide a report in PDF format of at least two pages, plus
one per extension. You want to summarise the workings of the code, and highlight
the interesting parts and difficulties you encountered.

In particular, describing how your strategy, and thus your code, evolved will be
beneficial.

### Code quality

The code quality will be the main evaluation criterion for your assignment.
You can think of this as a two-part marking scheme:

 1. Apply concepts from the course. The important thing here is that you need to
    actually use them somewhere. For example, defining a new type and its
    `Monad` instance, but then never actually needing to use it will not give
    you marks. (Note: using bind `(>>=)` for the sake of *using the `Monad`*
    when it is not needed will not count as "effective usage.")
 2. Have readable code, commented when necessary. Readable code means that you
    keep your lines at a reasonable length (< 80 characters). That you provide
    comments above non-trivial functions. And, that you comment sections of your
    code whose function may not be clear.
    
Remember, the point of comments is to give a *manual* rather than describe the
code. In the case of a function, you would explain how to use it rather than
what are the parameters, return types, etc.

### Memory and parsing

One of the key features of your player is the ability to keep track of the game.
To enable everyone to use their own datatypes, the game code will consider your
memory to be a `String`.

Handling complex data as strings is cumbersome. This means you will have to
implement *serialisation* and *deserialisation*. This will be done using a
*parser-combinator* -- of which you can see an explanation
[here](https://tgdwyer.github.io/parsercombinators/). The source code is
provided in `src/Parser/`. 

You can use the `Show` instance to serialise your data structures. However, you
must not use (or derive) the `Read` instance. We require you to use the
parser-combinator supplied to handle deserialisation.

Your memory should help you make decisions. This means your player needs to use
the memory to compute relevant parameters for choosing an action. Simply storing
past information is not sufficient. For example, your player could use the
memory to compute statistics about unseen cards, your opponent's hand, etc.

### Player

We will run a tournament [online](https://fit2102.monash) based on the code
provided. Except the interface, this will be the same game.

**Important**: Your rank in the tournament will not have a direct
impact on your mark. A high-performing player with spaghetti code will
be graded lower than an average, well-written player.

However, we will also upload a number of bots on the server. They will be
identifiable by having ids below 10. Having a higher rank than them will award
you marks:

 - *5%* for having a valid player, that is one which can play a game.
 - *5%* for having a continuing player, that is one which does not error
   during the tournament -- e.g., timeouts.
 - *5%* for beating at least one of the bots.
 - *5%* for beating all of the bots.

## Marking rubric

 - **Pass:** The code compiles without warnings and your player has some
   heuristic strategy (see Game AI below), you use some form of memory with
   parsing. The report supports the code.
 - **Credit:** You use the memory to store non-trivial information and have a
   clear report outlining your efforts.
 - **Distinction:** The code is well structured and uses some advanced concepts
   from the course -- higher order functions, function composition, monadic
   operations, etc.
 - **High Distinction:** The code does not contain any excess parts, the memory
   is used to store curated data about the game, the player can defeat all
   training opponents, and the documentation supports the submission.

Do note you can expect a higher mark with an average level AI with very neat
code, rather than a high-performing AI with spaghetti code.

### Game AI

The goal of this assignment is not for you to develop an AI which can compete
with [OpenAI](https://openai.com/) or
[AlphaGo](https://deepmind.com/research/case-studies/alphago-the-story-so-far).
The emphasis should be on code quality and applying functional concepts.
However, a more *advanced* AI usually requires more interesting code.

Below, you can find a non-exhaustive list of AI algorithms, ranked by
implementation difficulty, which you can use as reference. Note that a well
implemented heuristic player that is excellent with respect to all the criteria
above is sufficient for an HD. On the contrary, a complex Monte Carlo player
(see below) which has very bad code quality and makes no use of the memory may
very well not get a passing grade.

 - **Naïve AI:** tries to play its best card given the current state of the
   game, you can start by implementing one to make sure you respect the game's
   rules. However, this will not get a passing grade.
 - **Heuristic player:** has a procedure (heuristic) to determine the strength
   of its hand versus its opponent's and saves additional information about the
   game being played to enhance its decision at each turn.
 - **MinMax:**[^4] tries to minimise the maximum loss of a player by building a
   tree of possible moves and playing against itself. This method was developed
   for two-player, zero-sum game with perfect information. In this context, you
   will have to take into account the uncertain nature of the game.
 - **Probabilistic player:** will make use of probabilities to determine which
   cards have the highest chance of winning the game (i.e., appearing in the
   stock) or how good their opponent's hand is. It will make use of the memory
   to keep track of played cards and refine its calculations.

### Extensions

This assignment is fairly open-ended. You can achieve an HD with a solid player
and very neat code, but getting a high HD will require you to go beyond. Here,
you can find some ideas of what we consider extensions. All extensions need to
be supported by at least one additional page in your report. Feel free to come
up with your own.

And don't forget, you will only be awarded marks for extension work that extends
an already high quality submission. If the core of your submission is not
already HD-worthy, the extension will not grant you many marks.

#### Using logs to build a player

The game server (see below) will keep logs of your games against other players.
Reports for each game will come in two files named:

1. `<timestamp>.csv` the logs of the turns taken by each player, anonymised.
2. `<timestamp>-score.csv` the score for each round of the game, along with the
   action taken.

You can write a Haskell program to data-mine these reports and tailor some parts
of your player accordingly.

##### Turn file

``` csv
1,SA;CK;DK;S3;HJ;C9;S4;S5;SQ;D10,0,H5,SA,C3
```

Each file will be the record of *one game* -- so, multiple rounds. The file will
come without a header but here are the columns:

1. Round number. 
2. Cards in the player's hand -- the format is `<first char of suit><rank>`
   separated by ';'. *Note:* these are the cards *at the end of the turn,* so
   they include the drawn card -- as opposed to what your function receives.
3. Whether it is *your turn* to play -- '0' means your opponent's turn and '1'
   your turn.
4. The card at the top of the discard.
5. Which card was picked -- if it is not the same as the discard, then it came
   from the stock.
6. And, which card was discarded.

##### Score file

``` csv
1,0,12,Drop,0
```

Each file will record one round per row, formatted as:

1. Round number.
2. Your score.
3. Opponent's score -- in the same fashion as during the game.
4. Action taken.
5. Whether the action was called by you ('1') or your opponent ('0').

*Note*: if the action is "Drop," this means no player took an action before the
stock ran out.

#### Monte Carlo Tree Search 

Monte Carlo Tree Search (MCTS) is the fusion between (tree) search algorithms
such as minmax and using probabilities (Monte Carlo simulation) to determine the
branching in the search tree. It makes use of a *simulation phase* to explore
deeper. In this context, you can leverage the memory to save already explored
branches, or weight, etc. 

*Hint:* Building a MCTS player requires having access to a source of entropy for
side-effect-free random number generation; you can use your hand as it comes
from a shuffled deck.
   
#### Writing an extensive test suite

Testing in functional languages is often done semi-automatically. This is
because the test framework can leverage the type system to generate arbitrary
inputs -- think fuzzing. 

In the course material, we use [Doctest](https://github.com/sol/doctest#readme).
You may have seen lines starting with `prop>`. These mean "properties" and what
is great, for us programmers, is that we do not need to come up with inputs, the
testing framework does it itself.

The leading test framework in Haskell is
[QuickCheck](https://hackage.haskell.org/package/QuickCheck). It is actually
what is called in the `prop>` example above. Identifying properties (sometimes
called invariants) of your code can help you write better functions.

The only limitation here is that the test suite must be *extensive.* By that we
mean that you need to test more than one aspect of your functions (not just
"returns a `Card`"). Furthermore, you will need a compelling report showing how
you used the test suite to design your code -- think Test Driven Development,
where you determine the function's behaviour first rather than writing it and
adding tests for cases you think of.

### Plagiarism

<https://www.monash.edu/students/admin/policies/academic-integrity>

We will be checking your code against the rest of the class, and the
internet, using a plagiarism checker. Monash applies strict penalties
to students who are found to have committed plagiarism.

Any plagiarism will lead to a 0 mark for the assignment and will be
investigated by the staff.  There is a zero-tolerance policy in place at
Monash.  So be careful and report any collaboration with other students.

## Tournament server

We will run a server for the course at <https://fit2102.monash> with the
following pages:

 - [The uploader](https://fit2102.monash/uploader/): after logging in,
   this page will allow you to upload your code and compete in the
   tournament.
 - [The handout](https://fit2102.monash/resources/assignment.html):
   this document.
 - [The ladder](https://fit2102.monash/ladder.php): this
   page will display the scores of the last tournament run.
   
One thing to note is that the server only accept submissions as whole files. If
your code uses a multi-file structure, you will need to concatenate them into
your `Player.hs` before uploading.

Once you upload your player, you will see two links on the page:

 - `home.php`: shows your current ranking, last upload, and previous
   games played.
 - `status.php`: shows the status of your current upload. Furthermore, you can
   inspect your games by clicking on their number.

Before uploading your player, please check that the following runs:

```
stack exec staticgame
```

This will run a single game with two instances of your player. You can modify
this code (found in `staticgame/Main.hs`) to run different versions of your
code.

You cannot import external libraries because the server cannot know about them.
In a nutshell, you cannot edit the `stack.yaml` or the `package.yaml`.

The code provided uses the Safe pragma[^2] to make sure the code you use is okay
to run. It is also compiled with the `-Werror` flag which means that all
warnings will throw errors and prevent your code from compiling. So make sure
you run the test suite before you upload your player.

### Summary of tournament submission rules

 - *Respect the rules:* your player must always play valid actions or it will be
   eliminated.
 - *Be timely:* to give everyone a fair chance, your functions must all return
   in under one second.
 - *Be safe:* your player must compile with all flags provided, including the
   `import safe`.
 - *Single file:* your code must be submitted on the server as a single file.

[^2]: More info at
    [SafeHaskell](https://ghc.haskell.org/trac/ghc/wiki/SafeHaskell), but this
    should not hinder your work.

[^4]: https://en.wikipedia.org/wiki/Minimax

[^5]: Examples taken from [Wikipedia](https://en.wikipedia.org/wiki/Gin_rummy).

[^6]: This means a deck with four suits (clubs, diamonds, spades and hearts)
    with the following ranking: King, Queen, Jack, 10, 9, 8, 7, 6, 5, 4, 3, 2,
    Ace.

[^7]: You can compute the optimal arrangement of melds given a hand, but this
    may be [expensive](https://en.wikipedia.org/wiki/Subset_sum_problem).
