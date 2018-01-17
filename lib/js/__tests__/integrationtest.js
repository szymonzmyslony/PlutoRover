// Generated by BUCKLESCRIPT VERSION 1.9.3, PLEASE EDIT WITH CARE
'use strict';

var Jest             = require("bs-jest/lib/js/src/jest.js");
var PlutoRover$Pluto = require("../src/PlutoRover.js");

var board = PlutoRover$Pluto.createEmptyBoard(100, 100);

var plutoOne = PlutoRover$Pluto.createPluto(/* tuple */[
      0,
      0
    ], /* North */0, board);

var plutoOutOfBoard = PlutoRover$Pluto.createPluto(/* tuple */[
      1,
      -1
    ], /* South */1, board);

var movesBasic = "F, F, F, F, R, R, F, F, F, F";

var movesComplicated = "F, L, F";

describe("combine rotation plus moving pluto tests", (function () {
        Jest.test("it correctly moves, rotates 180 degrees and goes back to the same place", (function () {
                return Jest.Expect[/* toEqual */12](plutoOne, Jest.Expect[/* expect */0](PlutoRover$Pluto.evaluateMoves(movesBasic, plutoOne)));
              }));
        return Jest.test("it goes back to the same place if rotating", (function () {
                      return Jest.Expect[/* toEqual */12](plutoOutOfBoard, Jest.Expect[/* expect */0](PlutoRover$Pluto.evaluateMoves(movesComplicated, plutoOne)));
                    }));
      }));

exports.board            = board;
exports.plutoOne         = plutoOne;
exports.plutoOutOfBoard  = plutoOutOfBoard;
exports.movesBasic       = movesBasic;
exports.movesComplicated = movesComplicated;
/* board Not a pure module */