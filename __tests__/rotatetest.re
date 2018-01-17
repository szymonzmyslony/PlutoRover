open PlutoRover;
open Jest;
open Expect;

let board = createEmptyBoard 100 100;
let plutoOne = createPluto (0, 0) North board;
let plutoRotated = createPluto (0, 0) South board;
let movesBasic = "RR";
let moveReturn= "RLRL";

describe
  "basic rotation pluto tests"
  (
    fun _ => {
      test
        "it rotates correctly without moving"
        (
          fun _ =>
          expect (evaluateMoves movesBasic plutoOne) |>
          toEqual plutoRotated
        );

        test
        "it goes back to the same place if rotating"
        (
          fun _ =>
          expect (evaluateMoves moveReturn plutoOne) |>
          toEqual plutoOne
        );
 
    }
  );




