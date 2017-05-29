TARGET=ex1.aig \
	ex2.aig \
	ex3.aig \
	record.aig \
	array.aig \
	union.aig \
	conditionals.aig \
	rising_edge.aig \
	# Empty last line

%.aig: %.spec ../src/speculoosCompiler.byte
	../src/speculoosCompiler.byte $< -o $@
	diff $@ $@.ref

test: $(TARGET)

clean:
	rm *.aig

