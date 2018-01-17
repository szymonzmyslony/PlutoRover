open PlutoRover;
open Jest;
open Expect;

let board = createEmptyBoard 100 100;
let plutoOne = createPluto (0, 0) North board;
let plutoOneMoved = createPluto (4, 0) North board;
let plutoTwo = createPluto (10, 10) South board;
let movesBasic = "F,F,F,F";
let moveReturn= "F,B,F,B";

describe
  "basic pluto tests"
  (
    fun _ => {
        test
        "it parses string into correct types"
        (
          fun _ =>
            expect (mapCommandToMove "FBAFB") |>
            toEqual [F, B, NotValid, F, B]
        );

      test
        "it moves correctly in a straigth line"
        (
          fun _ =>
          expect (evaluateMoves movesBasic plutoOne) |>
          toEqual plutoOneMoved
        );

        test
        "it goes back to the same place if going backward"
        (
          fun _ =>
          expect (evaluateMoves moveReturn plutoOne) |>
          toEqual plutoOne
        );
 
    }
  );




