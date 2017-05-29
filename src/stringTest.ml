open Expression

let char_width = 8
let char_type = Type.int char_width
let max_string_length = 256
let log_max_string_length = 8
  
let string_type =
  Type.record
    ["length", Type.int log_max_string_length;
     "data", Type.array char_type max_string_length]

class virtual string =
object(self)
  val virtual value : Expression.t
      
  method get_length = field value "length"
    
  method get_data = field value "data"

  method at index = get self#get_data index
end
  
class string_symbol var_name =
object(self)
  inherit string
  val value = var var_name string_type
end

class string_constant s =
object(self)
  inherit string
  val value =
    let length = String.length s in
    let arr = Array.make length (int 0) in
    for i = 0 to String.length s - 1
    do
      arr.(i) <- int (int_of_char s.[i])
    done;
    record ["length", int length; "data", array arr]
end
  
let string_equals s1 s2 =
  let index = var "index_equals" (Type.int log_max_string_length) in
  conj
    (equals s1#get_length s2#get_length)
    (forall index (implies (less index s1#get_length) (equals (s1#at index) (s2#at index))))

(* formulas stating that s3 is s1.concat(s2) *)
let concat s1 s2 s3 =
  (* |s1| + |s2| = |s3| *)
  let a1 = equals (add s1#get_length s2#get_length) s3#get_length in
  (* forall i1 < |s1|. s1[i1] = s3[i1] *)
  let index1 = var "index1" (Type.int log_max_string_length) in
  let a2 = forall index1
    (implies (less index1 s1#get_length) (equals (s1#at index1) (s3#at index1)))
  in
  (* forall i2 < |s2|. s2[i2] = s3[i2+|s1|] *)
  let index2 = var "index2" (Type.int log_max_string_length) in
  let a3 = forall index2
    (implies (less index2 s2#get_length) (equals (s2#at index2) (s3#at (add index2 s1#get_length))))
  in
  conj (conj a1 a2) a3
  
let main =
  let string1 = new string_symbol "string1" in
  let i1 = var "index1" (Type.int log_max_string_length) in
  let b = equals (string1#at i1) (int 32) in
  print_endline (to_string b);
  let qf = forall i1 b in
  print_endline (to_string qf);
  Cudd.init 100;
  (match qf with
  | EBool b ->
     let bdd = ExprToBdd.expr_to_bdd b in
     print_endline "writing stringTest.dot";
     Cudd.dumpDot "stringTest.dot" bdd
  | _ -> failwith "not a boolean");

  let sta = new string_constant "aa" in
  let eq = string_equals string1 sta in
  print_endline "equality :";
  print_endline (to_string eq);
  (match eq with
  | EBool b ->
     let bdd = ExprToBdd.expr_to_bdd b in
     print_endline "writing stringTest.eq.dot";
     Cudd.dumpDot "stringTest.eq.dot" bdd);

  let form = concat string1 string1 sta in
  print_endline "formula :";
  print_endline (to_string form);
  (match form with
  | EBool b ->
     let bdd = ExprToBdd.expr_to_bdd b in
     print_endline "writing stringTest.form.dot";
     Cudd.dumpDot "stringTest.form.dot" bdd);
  ()
