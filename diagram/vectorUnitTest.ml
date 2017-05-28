open Vector
  
let main =
  let v = new vector 0 in
  v#push_back 2;
  Printf.printf "front %d\n" v#front;
  v#push_back 3;
  Printf.printf "back %d\n" v#back;
  v#push_front 1;
  Printf.printf "front %d\n" v#front;
  v#push_front 0;
  Printf.printf "front %d\n" v#front;
  Printf.printf "size %d\n" v#size;
  for i = 0 to v#size - 1
  do
    Printf.printf "get %d = %d\n" i (v#get i)
  done
