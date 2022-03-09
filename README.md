# Longest Possible Wordle Sequence

In February 2022, [Jane Street ran a contest](https://www.janestreet.com/puzzles/eldrow-index/) to find the longest possible sequence of words to enter in [Wordle](https://www.nytimes.com/games/wordle/index.html) before the game forces you to win. To make it non-trivial, you can only enter a word that is possibly a winner given all known information. Only the 2,315 possible winning words are allowed.

## My Solution

My solution was to basically brute force it by looking at every possible ordering of words, and to do so in a reasonable amount of time I spent a WHILE working on the performance. All the performance gains were either in the form of pruning the search tree:

 - Early exiting if a functionally identical state has been attempted
 - Early exiting if there aren't enough possible words left to beat the current high score

And general performance tweaks:

 - Caching results of common functions
   - E.g. caching the mapping of current constraint -> possible next words, which allows us to iterate over a much smaller set of words than the full 2,315 at each step
 - Removing all* dynamic allocations
   - *As you'll notice, I still use `vector` and `unordered_map` extensively, but they are all either not in a hot loop (e.g. globals) or cached so that they are only allocated once for a given key
   - This results in zero dynamic allocations once "warmed up", which doesn't take long
 - Using bitmasks to represent sets of letters (and then comparing sets with bitwise operations)
 - Every `Word` is just a `const char*` that points into the same array, so strings are effectively [interned](https://en.wikipedia.org/wiki/String_interning) and comparison can be done by comparing pointers
 - A custom function to be able to hash a partial constraint to a 128-bit int

Besides performance, the trickiest part was actually implementing the correct constraint logic. It took a few tries to get right because there are a few tricky edge cases involving guesses and solutions with multiple of the same letter.

## Results

I was the [5th person](https://www.janestreet.com/puzzles/eldrow-solution/) to submit the perfect solution (any sequence of length 16).

As a response to the line:
> We are certainly happy with our 16-length Eldrow sequences, and curious if some magical 17-length sequence is lurking out there…

I am pretty sure 16 is the maximum as I've checked every sequence exhaustively.

The sequence I submitted was:

    GAZER INNER QUEER ODDER BOXER FOYER HOVER JOKER WOOER LOWER MOWER POWER ROWER SOWER TOWER COWER (16) in 216 seconds

Which you can get yourself by running:

    ./eldrow.cpp 453 454

Or you can find the longest sequence for *every* possible answer with just:

    ./eldrow.cpp

(This will probably take a few hours! Though you can see the progress live so you can roughly guess how far you are into it)

## Code Quality

The usual disclaimer: I wrote this code without ever planning on releasing it, so it's pretty rough around the edges. Not to mention even more unreadable due to the hours I spent squeezing every last drop of performance out of it.
