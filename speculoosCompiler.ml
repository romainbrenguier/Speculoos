open Speculoos

let main = 
  let inch = open_in Sys.argv.(1) in
  let spec = Parser.parse_inch inch in
  close_in inch;
  Cudd.init 30;
  Speculoos.compile spec;
  Cudd.quit ()


