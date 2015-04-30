
let main = 
  let aiger = Aiger.read_from_file Sys.argv.(1) in
  let stream = lexer (Stream.of_string Sys.argv.(2)) in
  let dec,spec = Parser.parse aiger stream in
  Speculog.print_aiger (Speculog.add_synthesized dec spec aiger)


