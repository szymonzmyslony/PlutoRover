open PlutoRover;
open Jest;
open Expect;

let board = createEmptyBoard 100 100;
let plutoOne = createPluto (0, 0) North board;
let plutoOneRotated = createPluto (0, 0) South board;

let plutoOutOfBoard = createPluto (1, -1) West board;
let movesBasic = "F, F, F, F, R, R, F, F, F, F";
let movesComplicated= "F, L, F";

describe
  "combine rotation plus moving pluto tests"
  (
    fun _ => {
      test
        "it correctly moves, rotates 180 degrees and goes back to the same place"
        (
          fun _ =>
          expect (evaluateMoves movesBasic plutoOne) |>
          toEqual plutoOneRotated
        );
        test
        "it goes off the board if start at origin and then goes straight and turns left"
        (
          fun _ =>
          expect (evaluateMoves movesComplicated plutoOne) |>
          toEqual plutoOutOfBoard
        );
 
    }
  );

