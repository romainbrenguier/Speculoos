
speculoosCompiler: ocaml-aiger ocaml-cudd install_cudd src
	make -C ocaml-aiger/
	make -C ocaml-cudd/
	make -C src/
	cp src/speculoosCompiler.byte speculoosCompiler

install_cudd: ocaml-cudd/cudd-2.5.0/cudd/libcudd.a

ocaml-cudd/cudd-2.5.0/cudd/libcudd.a: ocaml-cudd/cudd-2.5.0/cudd/cudd.h
	sh ocaml-cudd/install_cudd.sh
