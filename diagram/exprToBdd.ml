open Expr

let rec expr_to_bdd = function
  | ExprTrue     -> Cudd.bddTrue()
  | ExprFalse    -> Cudd.bddFalse()
  | ExprSymbol k -> Cudd.ithVar k#get_id
  | ExprNot e    -> Cudd.bddNot (expr_to_bdd e)
  | ExprConjunction l when l#empty -> Cudd.bddTrue()
  | ExprDisjunction l when l#empty -> Cudd.bddFalse()
  | ExprConjunction l -> Util.reduce Cudd.bddAnd (Vector.map_to_list expr_to_bdd l) 
  | ExprDisjunction l -> Util.reduce Cudd.bddOr (Vector.map_to_list expr_to_bdd l)

  | ExprForall (vl,e) ->
    let variables = Vector.map_to_list (fun k -> k#get_id) vl in
    let cube = Cudd.make_cube variables in
    Cudd.bddUnivAbstract (expr_to_bdd e) cube

  | ExprExists (vl,e) ->
    let variables = Vector.map_to_list (fun k -> k#get_id) vl in
    let cube = Cudd.make_cube variables in
    Cudd.bddExistAbstract (expr_to_bdd e) cube
