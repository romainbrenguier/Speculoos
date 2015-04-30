open Expr

let size_max l = 
  let rec aux accu = function | [] -> accu | a :: l -> aux (max accu (Array.length a)) l
  in aux 0 l

(* Defines an Adder of the size of the given parameter *)  
let add_1 a b c_in s c_out = 
  Expr.of_list
    [ 
      s == (a ^ b) ^ c_in;
      c_out == ((a & b) || (a & c_in) || (b & c_in)) ;
    ]

let add a b out = 
  let size = size_max [a; b; out] in
  let _,w = wire "w" size in
  Expr.exists (Array.to_list w)
    (
      Expr.of_list
	[
	  add_1 a.(0) b.(0) False out.(0) w.(0);
	  Expr.for_each [1,(size-2)]
			(fun j -> 
			 add_1 (get a j) (get b j) w.(j-1) (get out j) w.(j));
	  add_1 (get a (size-1)) (get b (size-1)) w.(size-2) (get out (size-1)) (get out size)
	]
    )

let log i = 
  let rec aux i =
    if i < 2 then 1
    else 1 + aux (i/2)
  in aux i

let exp i =
  let rec aux i =
    if i < 1 then 1
    else 2 * aux (i-1)
  in aux i
  
let int a = 
  let size = log a in
  Array.init size (fun i -> if a / (exp i) mod 2 = 0 then Expr.False else Expr.True)


let equal a b =
  let size = size_max [a; b] in
  Expr.for_each [0,(size-1)]
    (fun j ->  (get a j) == (get b j))

let ( === ) a b = equal a b

let bitwise_and a b out = 
  let size = size_max [a; b; out] in
  Expr.for_each [0,(size-1)] (fun j -> get out j == (get a j & get b j))

let bitwise_or a b out = 
  let size = size_max [a; b; out] in
  Expr.for_each [0,(size-1)] (fun j -> get out j == (get a j || get b j))

let bitwise_xor a b out = 
  let size = size_max [a; b; out] in
  Expr.for_each [0,(size-1)] (fun j -> get out j == (get a j ^ get b j))

let ite condition t e =
  ((condition & t) || ((not condition) & e))

(*
let module_counter size =
  let buf = Buffer.create 500 in
  Printf.bprintf buf "input target;\n";
  Printf.bprintf buf "reg [%d:0] counter;\n" (size-1);
  Printf.bprintf buf "resize[size_in=1,size_out=%d](a=target,out=resized_target);\n" size;
  Printf.bprintf buf "adder[size=%d](a=resized_target,b=counter,s=next_counter);\n" size;
  Printf.bprintf buf "counter <- next_counter;\n";
  SpeculogParser.parse_module env (Buffer.contents buf)

let env = Environment.add_parametric_module env "counter" (fun f -> module_counter (get_int (f "size"))) 
 *)
(*
let int_to_bool_list size value =
  let rec aux accu nb i = 
    if nb = size 
    then accu 
    else aux ((i mod 2 = 1) :: accu) (nb+1) (i/2)
  in aux [] 0 value

let assign out size value = 
  let expr,nb = 
    List.fold_left
      (fun (expr,index) bool -> 
       var out (index-1) == (if bool then True else False)
      ) (int_to_bool_list size value)
  in
  Constraint.of_exprs
    (
    ) size 



let module_equal_1 value = 
  let buf = Buffer.create 500 in
  Printf.bprintf buf "input a;\n";
  Printf.bprintf buf "output out;\n";
  if value = 1 
  then 
    Printf.bprintf buf "out = a;\n"
  else 
    Printf.bprintf buf "out = !a;\n";
  SpeculogParser.parse_module env (Buffer.contents buf)

let module_equal size value =
  if size = 1 then module_equal_1 value
  else
  let buf = Buffer.create 500 in
  Printf.bprintf buf "input [%d:0] a;\n" (size-1);
  Printf.bprintf buf "output out;\n";
  Printf.bprintf buf "out = ";
  let hd :: tl = int_to_bool_list size value in
  if hd
  then Printf.bprintf buf " (a<%d>) " (size-1)
  else Printf.bprintf buf " (! a<%d>) " (size-1);
  let zero = List.fold_left
    (fun index bool -> 
      if bool 
      then 
	Printf.bprintf buf "& (a<%d>)" (index-1)
      else
	Printf.bprintf buf "& (! a<%d>)" (index-1);
      index-1
    ) (size-1) tl
  in
  Printf.bprintf buf ";\n";
  SpeculogParser.parse_module env (Buffer.contents buf)

let env = Environment.add_parametric_module env "equal" (fun f -> module_equal (get_int (f "size")) (get_int (f "value")))

let module_open filename = Aiger.read_from_file filename

let env = Environment.add_parametric_module env "open" (fun f -> module_open (get_string (f "file")))

(* The following modules use previously declared modules  *)
let module_switch nb size = 
  let buf = Buffer.create 100 in
  for i = 0 to (nb-1) 
  do
    if size > 1
    then 
      Printf.bprintf buf "input [%d:0] then%d;\n" (size-1) i
    else 
      Printf.bprintf buf "input then%d;\n" i;
    Printf.bprintf buf "input case%d;\n" i;
  done;
  if size > 1 
  then Printf.bprintf buf "output [%d:0] out;\n" (size-1)
  else Printf.bprintf buf "output out;\n";
  Printf.bprintf buf "assign[size=%d,value=0](out=w0);\n" size;
  for i = 0 to (nb-1) 
  do
    Printf.bprintf buf "if[size=%d](condition=case%d,then=then%d,else=w%d,out=w%d);\n" size i i i (i+1);
  done;
  if size = 1 
  then 
    Printf.bprintf buf "out = w%d;\n" nb
  else
    for j = 0 to size - 1 
    do Printf.bprintf buf "out<%d> = w%d<%d>;\n" j nb j done;
  Common.debug (Buffer.contents buf);
  SpeculogParser.parse_module env (Buffer.contents buf)

let env = Environment.add_parametric_module env "switch" (fun f -> module_switch (get_int (f "nb")) (get_int (f "size")))



let module_multiplexer size_select nb size =
  let buf = Buffer.create 100 in
  if size_select > 1
  then
    Printf.bprintf buf "input [%d:0] select;\n" (size_select-1)
  else
    Printf.bprintf buf "input select;\n";
  if size > 1
  then
    Printf.bprintf buf "output [%d:0] out;\n" (size-1)
  else     
    Printf.bprintf buf "output out;\n";

  for i = 0 to (nb-1) 
  do
    if size > 1
    then 
      Printf.bprintf buf "input [%d:0] input%d;\n" (size-1) i
    else
      Printf.bprintf buf "input input%d;\n" i;
  done;

  for i=0 to nb-1
  do
    Printf.bprintf buf "equal[size=%d,value=%d](a=select,out=select_equal_%d);\n" size_select i i
  done;
  Printf.bprintf buf "switch[size=%d,nb=%d](" size nb;
  for i=0 to nb-1
  do
    Printf.bprintf buf "case%d=select_equal_%d," i i;
    Printf.bprintf buf "then%d=input%d," i i;
  done;
  Printf.bprintf buf "out=out);\n";
  SpeculogParser.parse_module env (Buffer.contents buf)

let env = Environment.add_parametric_module env "multiplexer" (fun f -> module_multiplexer (get_int (f "size_select")) (get_int (f "nb")) (get_int (f "size")))


	  *)
