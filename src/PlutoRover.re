/* those are type definitions so that our program is type-safe */

type direction = North | South | East | West;

type move = F | B | R | L | NotValid ;

type square = Empty | Obstacle;

type position = (int, int);
type status = Ok| Obstacle;

type board = list (list square);

type pluto = {board, position, direction, status};
let mapCharToMove c => switch c {
    | 'F' => F
    | 'B' => B
    | 'L' => L
    | 'R' => R
    | _ => NotValid
};


/* there is no string to list of char function in reason, 
this is why we need recursive aproach */
let rec mapCommandToMove (command:string) => {
    switch command {
        | "" => []
        | command => {
            let length = String.length command;
            let rest = String.sub command 1 (length-1);
            let tail = mapCommandToMove rest;
            let firstCommand = mapCharToMove (String.get command 0);
            [firstCommand, ...tail]

        }
    };
};

/* core functionality of program */
let evaluateMove pluto m => {
    let {board, position, direction} = pluto;
    let heigth = List.length board;
    let width = List.length (List.nth board 0);
    let (x, y) = position;
    let newPosition = switch (direction, m) {
        | (North, F)
        | (South, B) => ( (x+1) mod heigth, y)  
        | (North, B) 
        | (South, F) => (x-1 + heigth mod heigth, y)
        | (East, F) 
        | (West, B) => (x, y+1 mod width )
        | (East, B)
        | (West, F) => (x, (y-1 + width) mod width  )
        | (_, _) => (x, y)
    };
    let boardRow = List.nth board x;
    let square = List.nth boardRow y;
    let (status, newPosition) = 
    switch square {
        | Empty => (Ok, newPosition)
        | Obstacle => (Obstacle, position)
    };
    let newDirection = switch (direction, m) {
        | (North, R) 
        | (South, L) => East
        | (North, L)
        | (South, R) => West
        | (West, L)
        | (East, R) => South
        | (West, R)
        | (East, L) => North
        | (_, _) => direction
    };

    let newPluto = {...pluto, position: newPosition, direction: newDirection, status};
    newPluto
 };


let evaluateMoves moves pluto => {
    let commands = mapCommandToMove moves;
    List.fold_left evaluateMove pluto commands
};


/* those are utility functions for testing and playing with API */
let createEmptyBoard heigth width => {
    let row = Array.make width Empty;
    let listRow = Array.to_list row;
    let board = Array.make heigth listRow;
    Array.to_list board
};


let createOneObstacleInRowBoard heigth width obstacle => {
    let row = Array.make width Empty;
    Array.set row obstacle Obstacle;
    let listRow = Array.to_list row;
    let board = Array.make heigth listRow;
    Array.to_list board
};

let createPluto startPosition startDirection board => 
{board, position:startPosition, direction:startDirection, status: Ok};