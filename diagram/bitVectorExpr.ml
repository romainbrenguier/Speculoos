open Expr
  
type bv_type = { signed : bool; width : int }
  
type t =
  | BitVectorConstant of int * bv_type
  | BitVectorSymbol of key * bv_type
  | BitVectorLiterals of Expr.t vector * bv_type
  | BitVectorSum of t vector * bv_type
  | BitVectorDiff of t * t * bv_type
  | BitVectorEquals of t * t * bv_type

let symbol_to_literals_table = Hashtbl.make 100

let find_symbol key bv_type =
  try
    let l = Hashtbl.find symbol_to_literals_table key in
    assert (l#size = bv_type.width);
    l
  with Not_found ->
    let v = new vector ExprFalse in
    for i = 0 to bv_type.width - 1
    do
      v#push_back (ExprSymbol (k#to_string ^ "$" ^ int_to_string i))
    done;
    Hashtbl.add symbol_to_literals_table key v;
    v

let convert_constant cst bv_type =
  let v = new vector ExprFalse in
  for i = 0 to bv_type.width - 1
  do
    let ith_bit = (cst >> i) & 1 in
    let ith_expr =
      match ith_bit with 1 -> ExprTrue | 0 -> ExprFalse | _ -> assert false
    in v#push_back ith_expr
  done;
  v

type add_1_result = { sum_bit : Expr.t; car_bit : Expr.t }
  
let add_1 a b c_in =
  let sum_bit = expr_xor (expr_xor a b) c_in in
  let car_bit =
    expr_or (expr_and a b) (expr_or (expr_and a c_in) (expr_and b c_in)))
  in { ~sum_bit; ~car_bit }

(* assume a and b are vectors of literals *)
let convert_sum a b =
  assert (a#size = b#size);
  let vec = new vector ExprFalse in
  let carry = ref ExprFalse in
  for i = 0 to a#size - 1
  do 
    let res = add_1 (a#get i) (b#get i) !carry in
    vec#push_back res.sum_bit;
    carry <- res.car_bit
  done;
  vec

let convert = function
  | BitVectorLiterals (e, bvt) -> e
  | BitVectorConstant (i, bvt) -> convert_constant i bvt
  | BitVectorSymbol (k, bvt) -> find_symbol (k, bvt)
  | BitVectorEquals (a, b) -> 
     let va = convert a and vb = convert b in
     assert (va#size = vb#size);
     let conj = new vector ExprFalse in
     for i = 0 to va#size - 1
     do
       conj#push_back (expr_equals (va#get i) (vb#get i))
     done;
     ExprConjunction conj
  | BitVectorSum (a, b) ->
     let va = convert a and vb = convert b in
     assert (va#size = vb#size);
     convert_sum va vb
     
