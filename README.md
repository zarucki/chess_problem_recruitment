# Chess challenge

----
## Problem definition
The problem is to find all unique configurations of a set of normal chess pieces on a chess board with
dimensions M×N where none of the pieces is in a position to take any of the others. Providing the number
of results is useful, but not enough to complete the assignment. Assume the colour of the piece does not
matter, and that there are no pawns among the pieces.

Write a program which takes as input:

* The dimensions of the board: M, N
* The number of pieces of each type (King, Queen, Bishop, Rook and Knight) to try and place on
the board.

As output, the program should list all the unique configurations to the console for which all of the pieces
can be placed on the board without threatening each other.

When returning your solution, please provide with your answer the total number of unique configurations
for a **7×7 board with 2 Kings, 2 Queens, 2 Bishops and 1 Knight. Also provide the time it took to
get the final score. Needless to say, the lower the time, the better**.

----
## Proposed solution
Simple BFS with small change that if we have multiple pieces of one type to place, we place them at once. We do this to avoid considering the same board configurations. This allows significantly narrowing down search space.

Further performance gains could be achieved by using the fact that when you find one solution, you can rotate it to get another one.

----

## Usage
This is simple sbt app. Main accepts problem in arguments:

    sbt "run 7 7 q q k k b b n"

This will return the default number of first solutions (won't return all of them).

You can also add additional number param to specify how many solutions you want printed:

    sbt "run 7 7 4 q q k k b b n"

The above will return 4 first solutions.

----
## My results
Running:

    sbt "run 7 7 4 q q k k b b n"

Gives following result on my T470 i5-6300u / 16GB:

    [info] Solving problem for: 7 x 7 (w x h) and pieces: B B K K N Q Q and printed solutions: 4
    [info]    -----------------------------
    [info]  6 |   |   |   |   |   |   | N |
    [info]    -----------------------------
    [info]  5 |   |   | Q |   |   |   |   |
    [info]    -----------------------------
    [info]  4 |   |   |   |   | K |   | B |
    [info]    -----------------------------
    [info]  3 |   |   |   |   |   |   |   |
    [info]    -----------------------------
    [info]  2 | B |   |   |   |   |   | K |
    [info]    -----------------------------
    [info]  1 |   |   |   |   |   |   |   |
    [info]    -----------------------------
    [info]  0 |   |   |   |   |   | Q |   |
    [info]    -----------------------------
    [info]      A   B   C   D   E   F   G  
    [info]    -----------------------------
    [info]  6 |   |   |   |   |   |   | N |
    [info]    -----------------------------
    [info]  5 |   |   | Q |   |   |   |   |
    [info]    -----------------------------
    [info]  4 | K |   |   |   |   |   | B |
    [info]    -----------------------------
    [info]  3 |   |   |   |   |   |   |   |
    [info]    -----------------------------
    [info]  2 | B |   |   |   |   |   | K |
    [info]    -----------------------------
    [info]  1 |   |   |   |   |   |   |   |
    [info]    -----------------------------
    [info]  0 |   |   |   |   |   | Q |   |
    [info]    -----------------------------
    [info]      A   B   C   D   E   F   G  
    [info]    -----------------------------
    [info]  6 |   |   |   |   |   |   | N |
    [info]    -----------------------------
    [info]  5 |   |   | Q |   |   |   |   |
    [info]    -----------------------------
    [info]  4 |   |   |   |   |   |   | B |
    [info]    -----------------------------
    [info]  3 |   |   |   | K |   |   |   |
    [info]    -----------------------------
    [info]  2 | B |   |   |   |   |   | K |
    [info]    -----------------------------
    [info]  1 |   |   |   |   |   |   |   |
    [info]    -----------------------------
    [info]  0 |   |   |   |   |   | Q |   |
    [info]    -----------------------------
    [info]      A   B   C   D   E   F   G  
    [info]    -----------------------------
    [info]  6 | K |   |   |   |   |   | N |
    [info]    -----------------------------
    [info]  5 |   |   | Q |   |   |   |   |
    [info]    -----------------------------
    [info]  4 |   |   |   |   |   |   | B |
    [info]    -----------------------------
    [info]  3 |   |   |   |   |   |   |   |
    [info]    -----------------------------
    [info]  2 | B |   |   |   |   |   | K |
    [info]    -----------------------------
    [info]  1 |   |   |   |   |   |   |   |
    [info]    -----------------------------
    [info]  0 |   |   |   |   |   | Q |   |
    [info]    -----------------------------
    [info]      A   B   C   D   E   F   G  
    [info] Total solutions: 3063828. Found in time: 21574 ms

