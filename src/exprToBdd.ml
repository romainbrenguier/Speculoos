let bdd_of_list cl =
  List.fold_left (fun accu x -> Cudd.bddAnd accu x) (Cudd.bddTrue()) cl

let rec expr_to_bdd expr = match expr with
  | Boolean.EVar x -> AigerBdd.bdd_of_symbol x
  | Boolean.EForall (vl,e) ->
     let variables =
       List.fold_left
	 (fun accu e ->
	   match e with
	   | Boolean.EVar x -> x :: accu
	   | _ ->
	      failwith ("In ExprToBdd.expr_to_bdd : universal "^
			 "quantification on expressions that are not variables")
	  ) [] vl
     in
     let cube = AigerBdd.make_cube variables in
     Cudd.bddUnivAbstract (expr_to_bdd e) cube

  | Boolean.EExists (vl,e) ->
     let variables =
       List.fold_left
	 (fun accu e ->
	   match e with
	   | Boolean.EVar x -> x :: accu
	   | _ ->
	      failwith ("In Speculog.Constraint.of_expr: existential"^
			"quantification on expressions that are not variables")
	 ) [] vl
     in
     let cube = AigerBdd.make_cube variables in
     Cudd.bddExistAbstract (expr_to_bdd e) cube

  | Boolean.ENot e -> Cudd.bddNot (expr_to_bdd e)
  | Boolean.EAnd (hd :: tl) ->
     List.fold_left Cudd.bddAnd (expr_to_bdd hd) (List.map expr_to_bdd tl)

  | Boolean.EAnd [] -> Cudd.bddTrue()
  | Boolean.EOr (hd :: tl) ->
     List.fold_left Cudd.bddOr (expr_to_bdd hd) (List.map expr_to_bdd tl)

  | Boolean.EOr [] -> Cudd.bddFalse()
  | Boolean.EList el -> bdd_of_list (List.map expr_to_bdd el)
  | Boolean.True -> Cudd.bddTrue()
  | Boolean.False -> Cudd.bddFalse()
