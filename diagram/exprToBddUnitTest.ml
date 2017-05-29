open ExprToBdd
  
let main =
  Cudd.init 100;
  let bdd = expr_to_bdd ExprUnitTest.expr in
  print_endline "writing exprToBddUnitTest.dot";
  Cudd.dumpDot "exprToBddUnitTest.dot" bdd;
  bdd
