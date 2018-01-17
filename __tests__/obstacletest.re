open PlutoRover;
open Jest;
open Expect;

let board:board = createOneObstacleInRowBoard 100 100 5;
let plutoOne = createPluto (0, 5) North board;
let movesBasic = "FFFFF";

let plutoMoved = createPluto (4, 5) North board;


describe
  "basic pluto tests"
  (
    fun _ => {
        test
        "it doesn't get through obstacle"
        (
          fun _ =>
            expect (evaluateMoves movesBasic plutoOne) |>
            toEqual plutoMoved
        );

 
 
    }
  );




