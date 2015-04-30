

all: simulation.byte synthax speculogSynth.byte

synthax: 
	ocamlbuild expression.cmx
	ocamlbuild speculog.cmx
	ocamlbuild aigerBdd.cmx
	camlp4o pa_extend.cmo q_MLast.cmo pr_o.cmo pa_speculog.ml -o pa_speculog.ppo 
	camlp4o pa_extend.cmo q_MLast.cmo pa_speculog.ml -o pa_speculog.ast
	ocamlbuild pa_speculog.cmo

simulation.byte: *.ml
	ocamlbuild simulation.byte

speculogSynth.byte: *.ml
	ocamlbuild speculogSynth.byte

regularExpression.byte: *.ml
	ocamlbuild regularExpression.byte

clean:
	ocamlbuild -clean
	rm -f *.ppo *.ppr *.ast

doc:
	ocamlbuild speculog.docdir/index.html

test: speculogSynth.byte
	./speculogSynth.byte examples/ex1.spec
	./speculogSynth.byte examples/ex2.spec
	./speculogSynth.byte examples/ex3.spec
	./speculogSynth.byte examples/record.spec
	./speculogSynth.byte examples/array.spec
	./speculogSynth.byte examples/union.spec
