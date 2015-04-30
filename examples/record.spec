var state : { x : int 4; y : int 4; time : int 4 };
var input : bool ;


state.x <- if state.y < 10 & input then state.x + 1 else state.x - 1;
state.y <- if state.y < 10 & input then state.y + 1 else state.y;
state.time <- state.time + 1;



