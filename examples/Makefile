TARGET=ex1.aig \
	ex2.aig \
	ex3.aig \
	record.aig \
	rising_edge.aig \
	conditionals.aig \
	union.aig \
	array.aig \
	# Empty last line

test: $(TARGET)

%.aig: %.spec ../src/speculoosCompiler.byte
	../src/speculoosCompiler.byte $< -o $@
	diff $@ $@.ref

#Initialize references, to be used carefuly
%.aig.ref: %.spec ../src/speculoosCompiler.byte
	../src/speculoosCompiler.byte $< -o $@
	
init_ref: ex1.aig.ref ex2.aig.ref ex3.aig.ref record.aig.ref\
	rising_edge.aig.ref \
	conditionals.aig.ref \
	union.aig.ref \
	array.aig.ref \
	# Empty last line

clean:
	rm *.aig

