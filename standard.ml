open Speculog 
open Expression

let adder i = 
  let a = var "a" (Type.int i) in
  let b = var "b" (Type.int i) in
  let c = var "c" (Type.int (i+1)) in
  functional_synthesis [c,add a b]


let product i = 
  let a = var "a" (Type.int i) in
  let b = var "b" (Type.int i) in
  let c = var "c" (Type.int (2 * i)) in
  functional_synthesis [c,mult a b]

let complex_product i = 
  let a = var "a" (Type.record ["real",Type.int i; "imaginary",Type.int i]) in
  let b = var "b" (Type.record ["real",Type.int i; "imaginary",Type.int i]) in
  let c = var "c" (Type.record ["real",Type.int (2*i); "imaginary",Type.int (2*i)]) in
  let real x = field x "real" in let complex x = field x "imaginary" in
  functional_synthesis 
    [
      real c, minus (mult (real a) (real b)) (mult (complex a) (complex b));
      complex c, add (mult (real a) (complex b)) (mult (complex a) (real b));
    ]

let assign ~size ~value ~output = 
  let out = var output (Type.int size) in
  functional_synthesis [out, int value]


let equal ~size ~value ~input ~output = 
  let a = var input (Type.int size) in
  let out = var output Type.bool in
  functional_synthesis [out, equals a (int value)]

let ite ~size ~condition ~t ~e ~output =
  let a = var t (Type.int size) in
  let b = var e (Type.int size) in
  let c = var condition Type.bool in
  let out = var output (Type.int size) in
  functional_synthesis [out, ite c a b] 
