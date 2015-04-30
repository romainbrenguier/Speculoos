var request : bool;
var state : ready | busy;


init state <- ready; 
updates state <- if state = ready & request then busy else ready;
