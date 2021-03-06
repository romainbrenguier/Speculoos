open Ocamlbuild_plugin
open Command

let ocaml_aiger="../../../ocaml-aiger/"
let ocaml_cudd="../../../ocaml-cudd/"
  
let () =
  dispatch 
    (
      function
      | After_rules ->
	flag ["doc";"use_ocaml-aiger"] (S[A "-I"; P(ocaml_aiger^"_build")]);
	flag ["ocaml";"compile";"use_ocaml-aiger"] (S[A "-I"; P(ocaml_aiger^"_build")]);
	flag
	  ["ocaml";"link";"use_ocaml-aiger";"byte"]
	  (S[P(ocaml_aiger^"_build/aiger.cma"); P(ocaml_aiger^"_build/aigerImperative.cma")]);
	flag
	  ["ocaml";"link";"use_ocaml-aiger";"native"]
	  (S[P "str.cmxa"; P(ocaml_aiger^"_build/aiger.cmx");
	     P(ocaml_aiger^"_build/aigerImperative.cmx")]);
	flag ["ocaml";"compile";"use_ocaml-cudd"] (S[A "-I"; P (ocaml_cudd^"_build/")]);
	flag ["ocaml";"compile";"syntax_extension"] 
	     (S[
		  A "-I"; P"+camlp4";A"-pp";
		  Quote (S[P"camlp4o"; P"pa_extend.cmo"; P"q_MLast.cmo"]);
		  A "-dtypes"
	     ]);
	flag ["ocaml";"compile";"my_syntax"] 
	     (S[
		  A "-dtypes";
		  A"-pp";
		  Quote (S[P"_build/pa_speculog.cmo";P"camlp4o";P"pa_extend.cmo";P"q_MLast.cmo"]);
	     ]);
	flag ["ocaml";"compile";"camlp4"] (S[A"-I";P"camlp4-lib-dir";A"-I";P"+camlp4"]);
	flag ["ocaml";"link";"use_ocaml-cudd";"byte"] 
	  (S [
	    A"-custom";
	    P(ocaml_cudd^"_build/cudd.o");
	    P(ocaml_cudd^"_build/cudd.cmo"); 
	    P(ocaml_cudd^"cudd-2.5.0/cudd/libcudd.a");
	    P(ocaml_cudd^"cudd-2.5.0/util/libutil.a");
	    P(ocaml_cudd^"cudd-2.5.0/epd/libepd.a");
	    P(ocaml_cudd^"cudd-2.5.0/mtr/libmtr.a");
	    P(ocaml_cudd^"cudd-2.5.0/st/libst.a");
	  ]);
	flag ["ocaml";"link";"use_ocaml-cudd";"native"] 
	  (S [
	    P(ocaml_cudd^"_build/cudd.o");
	    P(ocaml_cudd^"_build/cudd.cmxa"); 
	    P(ocaml_cudd^"cudd-2.5.0/cudd/libcudd.a");
	    P(ocaml_cudd^"cudd-2.5.0/util/libutil.a");
	    P(ocaml_cudd^"cudd-2.5.0/epd/libepd.a");
	    P(ocaml_cudd^"cudd-2.5.0/mtr/libmtr.a");
	    P(ocaml_cudd^"cudd-2.5.0/st/libst.a");
	  ]);
      | _ -> ()
    )
