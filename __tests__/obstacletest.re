open PlutoRover;

open Jest;

open Expect;

let board: board = createOneObstacleInRowBoard 10 10 5;

let plutoOne = createPluto (0, 0) East board;

let movesBasic = "FFFFFFFFFFFFFFFFF";

let plutoMoved = createPluto (0, 5) East board;

describe
  "basic obstacle pluto tests"
  (
    fun _ =>
      test
        "it doesn't get through obstacle"
        (
          fun _ =>
            expect (evaluateMoves movesBasic plutoOne) |> toEqual {...plutoMoved, status: Obstacle}
        )
  );