
let main = 
  let inch = open_in Sys.argv.(1) in
  let init,spec = Parser.parse_inch inch in
  close_in inch;
  Cudd.init 30;
  let spec = match init with [] ->  spec | _ -> Expression.init init spec in
  Speculog.print_aiger (Expression.functional_synthesis spec);
  Cudd.quit ()


