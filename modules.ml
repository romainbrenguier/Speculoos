open ParametricModule


let env = Environment.empty_env

(* Defines an Adder of the size of the given parameter *)  
let adder i = 
  let buf = Buffer.create 100 in
  Buffer.add_string buf "
module ADD;
input a;
input b;
input c_in;
output s;
output c_out;
s = (a ^ b) ^ c_in;
c_out = (a & b) | (a & c_in) | (b & c_in) ;
endmodule
";
  Printf.bprintf buf "input [%d:0] a;\n" (i-1);
  Printf.bprintf buf "input [%d:0] b;\n" (i-1);
  Printf.bprintf buf "output [%d:0] out;\n" (i);
  (*Printf.bprintf buf "output [%d:0] s;\n" (i+1);*)
  (*Printf.bprintf buf "wire [%d:0] w;\n" (i-2);*)
  Printf.bprintf buf "wfalse=0;\n";
  Printf.bprintf buf "ADD(a=a<0>,b=b<0>,s=out<0>, c_in=wfalse, c_out=w<0>);\n";
  for j = 1 to (i-2) do
    Printf.bprintf buf "ADD(a=a<%d>,b=b<%d>,s=out<%d>, c_in=w<%d>, c_out=w<%d>);\n" j j j (j-1) j;
  done;
  Printf.bprintf buf "ADD(a=a<%d>,b=b<%d>,s=out<%d>, c_in=w<%d>, c_out=out<%d>);\n" (i-1) (i-1) (i-1) (i-2) (i);
  (*Printf.bprintf buf "s = sw;";*)

  ModuleParser.parse (Buffer.contents buf)


let env = Environment.add_parametric_module env "adder" (fun f -> adder (get_int (f "size")))

let module_and size = 
  let buf = Buffer.create 100 in
  Printf.bprintf buf "input [%d:0] a;" (size-1);
  Printf.bprintf buf "input [%d:0] b;" (size-1);
  Printf.bprintf buf "output [%d:0] out;" (size-1);
  for j = 0 to size - 1 
  do Printf.bprintf buf "out<%d> = a<%d> & b<%d>;" j j j done;
  ModuleParser.parse (Buffer.contents buf)

let env = Environment.add_parametric_module env "and" (fun f -> module_and (get_int (f "size")))

let module_or size = 
  let buf = Buffer.create 100 in
  Printf.bprintf buf "input [%d:0] a;" (size-1);
  Printf.bprintf buf "input [%d:0] b;" (size-1);
  Printf.bprintf buf "output [%d:0] out;" (size-1);
  for j = 0 to size - 1 
  do Printf.bprintf buf "out<%d> = a<%d> | b<%d>;" j j j done;
  ModuleParser.parse (Buffer.contents buf)

let env = Environment.add_parametric_module env "or" (fun f -> module_or (get_int (f "size")))

let module_xor size = 
  let buf = Buffer.create 100 in
  Printf.bprintf buf "input [%d:0] a;" (size-1);
  Printf.bprintf buf "input [%d:0] b;" (size-1);
  Printf.bprintf buf "output [%d:0] out;" (size-1);
  for j = 0 to size - 1 
  do Printf.bprintf buf "out<%d> = (a<%d> ^ b<%d>);" j j j done;
  ModuleParser.parse (Buffer.contents buf)

let env = Environment.add_parametric_module env "xor" (fun f -> module_xor (get_int (f "size")))

let module_if_1 =
  let buf = Buffer.create 100 in
  Printf.bprintf buf "input then;";
  Printf.bprintf buf "input else;";
  Printf.bprintf buf "input condition;";
  Printf.bprintf buf "output out;";
  Printf.bprintf buf "out = (condition & then) | ((!condition) & else);\n";
  ModuleParser.parse (Buffer.contents buf)

let module_if size = 
  if size = 1 then module_if_1 
  else
  let buf = Buffer.create 100 in
  Printf.bprintf buf "input [%d:0] then;" (size-1);
  Printf.bprintf buf "input [%d:0] else;" (size-1);
  Printf.bprintf buf "input condition;";
  Printf.bprintf buf "output [%d:0] out;" (size-1);
  for j = 0 to size - 1 
  do Printf.bprintf buf "out<%d> = (condition & then<%d>) | ((!condition) & else<%d>);\n" j j j done;
  ModuleParser.parse (Buffer.contents buf)

let env = Environment.add_parametric_module env "if" (fun f -> module_if (get_int (f "size")))




let module_resize_1 size_out = 
  let buf = Buffer.create 100 in
  Printf.bprintf buf "input a;";
  Printf.bprintf buf "output [%d:0] out;" (size_out-1);
  Printf.bprintf buf "out<0> = a;";
  for j = 1 to size_out - 1
  do Printf.bprintf buf "out<%d> = 0;" j done;
  ModuleParser.parse (Buffer.contents buf)


let module_resize size_in size_out = 
  if size_in = 1 then module_resize_1 size_out
  else
    let buf = Buffer.create 100 in
    Printf.bprintf buf "input [%d:0] a;" (size_in-1);
    Printf.bprintf buf "output [%d:0] out;" (size_out-1);
    for j = 0 to min (size_in - 1) (size_out - 1)
    do Printf.bprintf buf "out<%d> = a<%d>;" j j done;
    if size_out > size_in 
    then 
      for j = size_in to size_out - 1
      do Printf.bprintf buf "out<%d> = 0;" j done;
    ModuleParser.parse (Buffer.contents buf)


let env = Environment.add_parametric_module env "resize" (fun f -> module_resize (get_int (f "size_in")) (get_int (f "size_out"))) 


let module_counter size =
  let buf = Buffer.create 500 in
  Printf.bprintf buf "input target;\n";
  Printf.bprintf buf "reg [%d:0] counter;\n" (size-1);
  Printf.bprintf buf "resize[size_in=1,size_out=%d](a=target,out=resized_target);\n" size;
  Printf.bprintf buf "adder[size=%d](a=resized_target,b=counter,s=next_counter);\n" size;
  Printf.bprintf buf "counter <- next_counter;\n";
  ModuleParser.parse_module env (Buffer.contents buf)

let env = Environment.add_parametric_module env "counter" (fun f -> module_counter (get_int (f "size"))) 

let int_to_bool_list size value =
  let rec aux accu nb i = 
    if nb = size 
    then accu 
    else aux ((i mod 2 = 1) :: accu) (nb+1) (i/2)
  in aux [] 0 value

let module_assign_1 value =
  let s = Printf.sprintf "output out;\nout = %d;\n" value in
  ModuleParser.parse s

let module_assign size value =
  if size = 1 then module_assign_1 value
  else
    let buf = Buffer.create 500 in
    
    Printf.bprintf buf "output [%d:0] out;\n" (size-1);
    
    List.fold_left
      (fun index bool -> 
	Printf.bprintf buf "out<%d> = %d;\n" (index-1) (if bool then 1 else 0);
	index-1
      ) size (int_to_bool_list size value);
    Common.debug (Buffer.contents buf);
    ModuleParser.parse (Buffer.contents buf)

let env = Environment.add_parametric_module env "assign" (fun f -> module_assign (get_int (f "size")) (get_int (f "value")))


let module_equal_1 value = 
  let buf = Buffer.create 500 in
  Printf.bprintf buf "input a;\n";
  Printf.bprintf buf "output out;\n";
  if value = 1 
  then 
    Printf.bprintf buf "out = a;\n"
  else 
    Printf.bprintf buf "out = !a;\n";
  ModuleParser.parse_module env (Buffer.contents buf)

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
  ModuleParser.parse_module env (Buffer.contents buf)

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
  ModuleParser.parse_module env (Buffer.contents buf)

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
  ModuleParser.parse_module env (Buffer.contents buf)

let env = Environment.add_parametric_module env "multiplexer" (fun f -> module_multiplexer (get_int (f "size_select")) (get_int (f "nb")) (get_int (f "size")))


