open PlutoRover;
open Jest;
open Expect;

let board = createEmptyBoard 100 100;
let plutoOne = createPluto (0, 0) North board;
let plutoOneRotated = createPluto (0, 0) South board;

let plutoOutOfBoard = createPluto (1, 99) West board;
let movesBasic = "F, F, F, F, R, R, F, F, F, F";
let movesComplicated= "F, L, F";

let movesFromExample= "F, F, R, F, F";

let plutoFromExample = createPluto (2, 2) East board;

let movesHundred = String.make 100 'F';

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
        "it doesn't go off the board if start at origin and then goes straight and turns left"
        (
          fun _ =>
          expect (evaluateMoves movesComplicated plutoOne) |>
          toEqual plutoOutOfBoard
        );
        test
        "it goes to the same place if moves 100 times forward"
        (
          fun _ =>
          expect (evaluateMoves movesHundred plutoOne) |>
          toEqual plutoOne
        );
        test
        "it behaves as described in the example"
        (
          fun _ =>
          expect (evaluateMoves movesFromExample plutoOne) |>
          toEqual plutoFromExample
        );
 
    }
  );

