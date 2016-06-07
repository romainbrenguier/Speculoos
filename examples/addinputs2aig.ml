let lexer = Genlex.make_lexer [".";"inputs"; "outputs";]

let parse stream = 
  let rec parse_inputs accu = parser
    | [< 'Genlex.Ident x >] -> parse_inputs (x :: accu) stream
    | [< >] -> accu
  in
  stream |> 
      parser
  | [< 'Genlex.Kwd "."; 'Genlex.Kwd "inputs" >] -> parse_inputs [] stream

let main = 
  if Array.length Sys.argv < 3 
  then Printf.printf "usage: %s <file.part> <file.aig>" Sys.argv.(0)
  else
    let inputs = Sys.argv.(1) |> open_in |> Stream.of_channel
  |> lexer |> parse 
    in
    let aig = Aiger.read_from_file Sys.argv.(2) in
    let old_inputs = List.map (Aiger.lit2string aig) aig.Aiger.inputs in
    let new_aig = List.fold_left 
      (fun a i -> 
	if List.mem i old_inputs then a
	else
	  let a,v = Aiger.new_var a in
	  Aiger.add_input a (Aiger.var2lit v) (i,None)) aig inputs in
    Aiger.write new_aig stdout

