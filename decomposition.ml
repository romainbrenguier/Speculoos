(* ocamlbuild -tag use_ocaml-aiger decomposition.byte *)

let aiger = Aiger.read_from_file Sys.argv.(1)

let decomp =
  let rec m node = match Aiger.lit2tag aiger node with
    | Constant x -> []
    | Input x | Latch (x,_) -> []
    | And (_,l,r) -> 
       let l = 
	 if Aiger.sign l then [Aiger.strip l]
	 else m l
       in 
       let r =
	 if Aiger.sign r then [Aiger.strip r]
	 else m r
       in 
       l @ r
    and n node = match Aiger.lit2tag aiger node with
    | Constant x -> []
    | Input x | Latch (x,_) -> [x]
    | And (_,l,r) -> 
       let l = if Aiger.sign l then [] else n l in 
       let r = if Aiger.sign r then [] else n r in 
       l @ r
  in 
  let err = List.hd aiger.Aiger.outputs in 
  let n1,m1 =   
    if Aiger.sign err 
    then [], [Aiger.strip err]
    else n err, m err 
  in

    print_endline "----N----";
    List.iter (fun l -> print_endline (string_of_int (Aiger.lit2int l))) n1;
    print_endline "----M----";
    List.iter (fun l -> print_endline (string_of_int (Aiger.lit2int l))) m1;
    
    List.iter 
      (fun mm ->
       let n2,m2 = n mm, m mm in
       Printf.printf "----N of %d----\n" (Aiger.lit2int mm);
       List.iter (fun l -> print_endline (Aiger.lit2string aiger l)) n2;
       Printf.printf "----M of %d----\n" (Aiger.lit2int mm);
       List.iter (fun l -> print_endline (string_of_int (Aiger.lit2int l))) m2
      ) m1
      
