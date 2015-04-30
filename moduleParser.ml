open ParametricModule
open Environment

(*let () = Dynlink.allow_unsafe_modules true

let load_module filename = 
  let ce = (try 
	      Dynlink.init () ;
	      Dynlink.loadfile filename;
	      (!current_env) ()
    with Dynlink.Error e -> print_endline (Dynlink.error_message e);
      (!current_env) ()
  ) in
  if ce = empty_env 
  then print_endline "!current_env empty";
  ce 
*)

let parse_declarations env string =
    let lexer = Genlex.make_lexer
    ["module";"endmodule";"load";"input";"output";"reg";"wire";"|";"&";"^";"!";"="; "("; ")";  "<-"; "<"; ">"; ";"; ","; ":";"[";"]";"true";"false"]  in
  let stream = lexer (Stream.of_string string) in
  try 
  let parse_option = parser
   | [< 'Genlex.Kwd "<"; 'Genlex.Int i; 'Genlex.Kwd ">" >] -> Some i
   | [< >] -> None 
  in
  let parse_size = parser
   | [< 'Genlex.Kwd "["; 'Genlex.Int i; 'Genlex.Kwd ":"; 'Genlex.Int j ; 'Genlex.Kwd "]" >] -> (i,j)
   | [< >] -> (0,0)
  in
  let rec parse_expr_remainder accu = parser 
   | [< 'Genlex.Kwd "|"; e = parse_expr >] -> EOr (accu,e)
   | [< 'Genlex.Kwd "&"; e = parse_expr >] -> EAnd (accu,e)
   | [< 'Genlex.Kwd "^"; e = parse_expr >] -> EXor (accu,e)
   | [< >] -> accu

  and parse_expr = parser
   | [< 'Genlex.Ident name ; i = parse_option; r = parse_expr_remainder (EVar (name,i)) >] -> r
   | [< 'Genlex.Int i ; r = parse_expr_remainder (if i = 0 then EFalse else ETrue) >] -> r
   | [< 'Genlex.Kwd "("; e = parse_expr; 'Genlex.Kwd ")"; r = parse_expr_remainder e >] -> r
   | [< 'Genlex.Kwd "!"; e = parse_neg; r = parse_expr_remainder (ENot e) >] -> r

  and parse_neg = parser
   | [< 'Genlex.Kwd "("; e = parse_expr; 'Genlex.Kwd ")" >] -> e
   | [< 'Genlex.Ident name ; i = parse_option; >] -> EVar (name,i)
  in


  let rec parse_renaming accu = parser
  | [< 'Genlex.Ident name1; i1 = parse_option; 'Genlex.Kwd "="; 'Genlex.Ident name2; i2 = parse_option; d = parse_renaming_end (((name1,i1),(name2,i2)) :: accu) >] -> d
  | [< >] -> failwith "waiting [ident[<i>]? = ident[<i>]? renaming]..."
  and parse_renaming_end accu = parser
      | [< 'Genlex.Kwd ")" >] -> accu
      | [< 'Genlex.Kwd ","; d = parse_renaming accu >] -> d
      | [< >] -> failwith "waiting ')' or ','..."
  in

  let parse_parameter_value = 
    parser
  | [< 'Genlex.Int i >] -> Int i
  | [< 'Genlex.String s >] -> String s
  | [< 'Genlex.Kwd "true" >] -> Bool true
  | [< 'Genlex.Kwd "false" >] -> Bool false
  | [< >] -> failwith "waiting [int] [string] [true] or [false]...."
  in
  let rec parse_parameters accu = 
    parser
  | [< 'Genlex.Ident name1; 'Genlex.Kwd "="; v = parse_parameter_value; d = parse_parameters_end ((name1,v) :: accu) >] -> d
  and parse_parameters_end accu = 
    parser
      | [< 'Genlex.Kwd "]" >] -> accu
      | [< 'Genlex.Kwd ","; d = parse_parameters accu >] -> d
      | [< >] -> failwith "waiting ']' or ',' ..."
  in

  let rec parse_dec env accu = parser 
| [< 'Genlex.Kwd "input"; s=parse_size; 'Genlex.Ident name; i = parse_option; 'Genlex.Kwd ";"; 
     d = parse_dec env (if s = (0,0) then Input (name,i) :: accu else 
	 List.rev_append (vector_input name (fst s) (snd s)) accu)
  >] -> d
| [< 'Genlex.Kwd "wire"; s=parse_size; 'Genlex.Ident name; i = parse_option; 'Genlex.Kwd ";"; d = parse_dec env (if s = (0,0) then Wire (name,i) :: accu else List.rev_append (vector_wire name (fst s) (snd s)) accu)
  >] -> d
| [< 'Genlex.Kwd "output"; s=parse_size; 'Genlex.Ident name; i = parse_option; 'Genlex.Kwd ";"; d = parse_dec env (if s = (0,0) then Output (name,i) :: accu else List.rev_append (vector_output name (fst s) (snd s)) accu)
  >] -> d

| [< 'Genlex.Kwd "reg"; s=parse_size; 'Genlex.Ident name; i = parse_option;'Genlex.Kwd ";"; d = parse_dec env (
  if s = (0,0) then Reg((name,i)) :: accu 
  else List.rev_append (vector_reg name (fst s) (snd s)) accu)
  >] -> d

| [< 'Genlex.Ident name; d = parse_after_ident env accu name >] -> d 
(*| [< 'Genlex.Kwd "load"; 'Genlex.String name; 'Genlex.Kwd ";";
     e = parse_dec (Environment.merge_env (load_module name) env) accu >] -> e*)

| [< 'Genlex.Kwd "module"; 'Genlex.Ident name; 'Genlex.Kwd ";";
     d = parse_dec env []; 'Genlex.Kwd "endmodule"; 
     e = parse_dec (add_module env name (of_declaration (List.rev d))) accu >] -> e
| [< >] -> accu

  and parse_after_ident env accu name =
    parser
      | [< 'Genlex.Kwd "["; parameters=parse_parameters []; 'Genlex.Kwd "("; renaming=parse_renaming []; 'Genlex.Kwd ";"; d = parse_dec env (UseParametricModule(find_parametric_module env name,parameters,renaming) :: accu)>] -> d
    | [< 'Genlex.Kwd "("; renaming=parse_renaming []; 'Genlex.Kwd ";"; d = parse_dec env (UseModule(find_module env name,renaming) :: accu)>] -> d
    | [< i = parse_option; d = parse_after_option env accu name i >] -> d

  and parse_after_option env accu name opt = 
    parser 
    | [< 'Genlex.Kwd "="; e=parse_expr; 'Genlex.Kwd ";"; d = parse_dec env (List.rev_append (expr_to_declarations (name,opt) e) accu) >] -> d
    | [< 'Genlex.Kwd "<-"; e=parse_expr; 'Genlex.Kwd ";"; d = parse_dec env (List.rev_append (expr_to_register_declarations (name,opt) e) accu) >] -> d
  in

  let res = List.rev (parse_dec env [] stream) in
  
  Stream.empty stream;
  res
  with 
    Stream.Failure | Stream.Error _ -> 
      Printf.eprintf "Warning: unexpected token %s in parse_declarations\n"
	(Common.next_token stream);
      Printf.eprintf "remaining of the stream: %s\n" (Common.remaining_tokens stream);
      raise Stream.Failure
	(*res*)


let parse_module env string = 
  let aag = of_declaration (parse_declarations env string) in 
  Aiger.add_comment aag string

let parse string = parse_module empty_env string
