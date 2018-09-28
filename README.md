# dominoes-puzzle
Solve a dominoes puzzle.

A dominoes puzzle is a rectangular board divided into square cells, each
cell containing a number of spots. A valid puzzle can be solved by placing a
complete deck of domino tiles onto the board.

A standard domino deck has 28 unique tiles, from double blank (no spots) to
double six. 28 tiles require 56 cells, such as those provided by a 7 x 8 board.

The principal function is `solve` exported by module `Dominoes`. A solution is a list of placed domino tiles (`[PlacedTile]`).
