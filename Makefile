
speculoosCompiler: ocaml-aiger src
	make -C ocaml-aiger/
	make -C src/
	cp src/speculoosCompiler.byte speculoosCompiler
