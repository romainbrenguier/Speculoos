open Vector
  
let main =
  let v = new vector 0 in
  
  v#push_back 3;
  Printf.printf "front %d\n" v#front;
  assert(v#front = 3);

  v#push_back 4;
  Printf.printf "back %d\n" v#back;
  assert(v#back = 4);

  v#push_front 2;
  Printf.printf "front %d\n" v#front;
  assert(v#front = 2);

  v#push_front 1;
  Printf.printf "front %d\n" v#front;
  assert(v#front = 1);

  v#push_front 0;
  Printf.printf "front %d\n" v#front;
  assert(v#front = 0);

  Printf.printf "size %d\n" v#size;
  assert(v#size = 5);
  
  for i = 0 to v#size - 1
  do
    Printf.printf "get %d = %d\n" i (v#get i);
    assert(v#get i = i);
  done;

  for i = 0 to 100
  do
    v#push_back (5+i);
  done;

  for i = 0 to 50
  do
    let j = v#pop_front in
    Printf.printf "pop %d = %d\n" i j; 
    assert(i = j);
  done;

  for i = 105 downto 50
  do
    let j = v#pop_back in
    Printf.printf "pop %d = %d\n" i j; 
    assert(i = j);
  done;

  v#set 50 1234;
  Printf.printf "get 50 %d\n" (v#get 50);
  assert(v#get 50 = 1234);

  print_endline "SUCCESS"
