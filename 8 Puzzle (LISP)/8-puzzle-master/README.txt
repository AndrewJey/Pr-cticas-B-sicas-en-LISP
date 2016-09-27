README for 8-puzzle
by Robert Colgan
erobertc92@gmail.com
rec2111@columbia.edu

This project is an implementation of an automated solver for the 8-puzzle, 
written in Lisp. It was created for COMS W4701 "Artificial Intelligence" taught
by Alexander J. Pasik at Columbia University in the fall of 2012. 

The 8-puzzle consists of a 3 by 3 board with eight numbered tiles and one empty
space, into which a neighboring numbered tile can be moved horizontally or 
vertically. The goal is to get all eight tiles into the configuration with the 
blank space in the upper right corner and the rest of the tiles in order from 1
to 8 filling up the rest of the board.

This program finds and reports the shortest possible sequence of moves to solve
an 8-puzzle starting from any random (solvable) configuration. It uses the A* 
algorithm to find the best solution with the fewest number of expanded nodes 
possible, using a heuristic to determine which nodes to expand. It also reports
the number of steps to reach the completed board and the number of nodes 
expanded. 

The program can be run in a Lisp environment by loading the single file 
8-puzzle.lisp and calling the function 8-puzzle with the first argument the 
initial state and the second argument the name of the function to use to 
calculate the heuristic (implemented heuristic functions are called misplaced, 
manhattan, and extracredit; they are discussed in more detail below), for 
example:
(8-puzzle '(7 5 0 4 2 1 3 6 8) #'manhattan)
which produces the output:
(("left" "down" "right" "up" "left" "left" "down" "right" "up" "left" "down" 
"down" "right" "up" "up" "left") 16 107)
indicating the sequence of moves to make to "move" the blank tile back to the 
upper right and restore the other tiles, as well as the number of moves 
necessary and the number of nodes expanded to find the solution. It can also
generate random starting states for an 8-puzzle by calling the 
make-random-board function with no arguments. The random case generator starts 
with an organized board and makes 100 random moves to create a randomized 
board.

The program implements the misplaced-tiles heuristic (which counts how many 
tiles are not in their target location) and the Manhattan distance heuristic 
(which sums the number of rows away from its target row plus number of columns 
away from its target column for every tile). It also implements a heuristic 
which dominates Manhattan distance for the extra credit (see below). 

The program represents an 8-puzzle configuration as an ordered list of nine 
numbers (representing the tiles, with 0 for the empty tile) with each number’s 
location in the list corresponding to its position on the board (the first item
in the list is the tile in the upper left; the last item is the tile in the 
lower right). 

For extra credit on the assignment, I implemented a heuristic that is based on
Manhattan distance but adds an additional qualification to dominate Manhattan 
distance. It takes the Manhattan distance value and also tests for linear 
conflicts—when two tiles are in their goal row or column but not in the correct
order, one tile will have to be moved away so the other can pass, then back up.
These two moves are not accounted for by Manhattan distance. My heuristic 
accounts for these necessary moves and adds the total number of these moves 
necessary to the result from Manhattan distance.