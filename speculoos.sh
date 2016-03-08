#!/bin/sh
TEMP_FILE="temporary.ml"
TEMP_BYTE="temporary.byte"

echo "open Speculog" >$TEMP_FILE
echo "open Expression" >>$TEMP_FILE

camlp4 -I +camlp4 -parser o -parser op -printer o _build/pa_speculog.cmo $1 >>$TEMP_FILE
ocamlbuild -tag camlp4 -tag use_ocaml-cudd -tag use_ocaml-aiger $2 $TEMP_BYTE && ./$TEMP_BYTE 
