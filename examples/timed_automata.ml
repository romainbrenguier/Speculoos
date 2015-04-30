(* ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/timed_automata.byte *)

type clock = Clock 

let clock name size = Speculog.reg name size
let clock_to_int x = ((Expr.cast x):(int Expr.t))

type order = Less | LessEq | Eq | GreaterEq | Greater
let order_to_expr = function
  | Less -> Expr.less
  | LessEq -> Expr.less_eq
  | Eq -> Expr.equals
  | GreaterEq -> Expr.greater_eq
  | Greater -> Expr.greater

type guard = clock Expr.t * order * int Expr.t

let guard_to_expr (c,o,i) dt =
  (order_to_expr o) (clock_to_int (Expr.add c dt)) i

let guards_to_expr gl dt =
  List.fold_left
    (fun accu g -> 
      Expr.conj accu (guard_to_expr g dt)
    ) (Expr.bool true) gl

type location = {id : int; invariant : bool Expr.t }
type transition = {src : int; dst : int; guards : guard list; resets : (clock Expr.t) list }
type t = {declarations : Speculog.declaration list; clocks: (clock Expr.t) list;
	  locations: location list; transitions: transition list;
	  maxval : int}


let to_spec auto =
  let d,location = Speculog.reg "location" (Expr.log (List.length auto.locations)) in
  let dl = d :: auto.declarations in
  let mlog = Expr.log auto.maxval in
  let d,dt = Speculog.input "dt" mlog in
  let dl = d :: dl in
  let max_location = List.fold_left (fun m l -> max m l.id) 0 auto.locations in
  let invariant_array = Array.make (max_location + 1) (Expr.bool true) in
  List.iter (fun l -> invariant_array.(l.id) <- l.invariant) auto.locations;
  let trans =
    List.fold_left 
      (fun accu t ->
	let clock_updates = 
	  List.fold_left
	    (fun accu c -> 
	      let cond =
		if List.mem c t.resets 
		then Expr.equals (clock_to_int (Expr.next c)) (Expr.int 0)
		else Expr.equals (Expr.next c) (Expr.add c dt) 
	      in Expr.conj accu cond
	  ) (Expr.bool true) auto.clocks
	in 
	let cond = 
	  Expr.conj (Expr.equals location (Expr.int t.src))
	    (Expr.conj (Expr.equals (Expr.next location) (Expr.int t.dst))
	       (Expr.conj clock_updates
		  (Expr.conj (guards_to_expr t.guards dt) invariant_array.(t.dst))))
	in
	Expr.disj accu cond
      ) (Expr.bool false) auto.transitions
  in 
  dl, trans



let test = 
  let decl,c = clock "clock1" 8 in
  let loc0 = {id=0; invariant = Expr.bool true} in
  let loc1 = {id=1; invariant = Expr.bool true} in
  let trans0 = {src=0; dst=0; guards=[c,LessEq,Expr.int 35]; resets=[]} in
  let trans1 = {src=0; dst=1; guards=[c,Greater,Expr.int 35]; resets=[]} in
  let trans2 = {src=1; dst=1; guards=[c,LessEq,Expr.int 23]; resets=[]} in
  let trans3 = {src=1; dst=1; guards=[c,Greater,Expr.int 23]; resets=[c]} in
  let auto = {declarations=[decl];clocks=[c];locations=[loc0;loc1];
	      transitions=[trans0;trans1;trans2;trans3]; maxval=511}
  in
  let decl,expr = to_spec auto in
  Cudd.init 100;
  (*print_endline (Expr.to_string expr);*)
  try
    let aiger = Speculog.synthesize decl [expr] in
    print_endline "writing aiger file to timed_auto.aig";
    Aiger.write_to_file aiger "timed_auto.aig"
  with Speculog.NonSynthesizable (e,f) ->
    let d,fail = Speculog.output "fail" 1 in
    let spec = Expr.ite f (Expr.equals fail (Expr.bool true)) (Expr.conj e (Expr.neg fail)) in
    let aiger = Speculog.synthesize (d::decl) [spec] in
    print_endline "writing aiger file to timed_auto.aig";
    Aiger.write_to_file aiger "timed_auto.aig"

(*  
let dc = 1
let vuc = 1
let cp = 50
let dd = 
  [|
    [|0; 186; 206; 161; 181; 151; 190; 160|];
    [|186; 0; 20; 36; 30; 42; 50; 53|];
    [|206; 20; 0; 50; 36; 58; 50; 64|];
    [|161; 36; 50; 0; 20; 10; 36; 20|];
    [|181; 30; 36; 20; 0; 30; 22; 28|];
    [|151; 42; 58; 10; 30; 0; 44; 22|];
    [|190; 50; 50; 36; 22; 44; 0; 30|];
    [|160; 53; 64; 20; 28; 22; 30; 0|]|]
let vehicleAt = Type.make "vehicleAt" (Type.Array(Type.Int(8),2))


let invariantIdle id = 
  gc <= (deadline - dd[vehicleAt[id]][0]) & (cost' = vuc)

let invariantDriving id = 
  c <= busy[id] && cost' = dc + vuc

let invariantUnloading id =
  c <= busy[id] && cost' = vuc

let invariantDrivingHome id =
  c <= busy[id] && cost' = dc + vuc

let invariantHome id = 
  cost' = 0

let transition0 = 
  location = id0 && 
  next location = id1 &&
  drive[id] &&
  next c = 0

input unload[id]

let transition1 =
  location = id1 &&
  next location = id2 &&
  unload[id] &&
  c = 0
  
let main =
  transition0 || transition1
*)
