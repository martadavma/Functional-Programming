#!/usr/bin/env bash

ocamlfind ocamlc -i -package base imp.ml > imp.mli
ocamlfind ocamlc -c -package base imp.mli
ocamlfind ocamlc -c -package base imp.ml

if [[ -f "impparser.mly" ]]; then
    menhir --infer impparser.mly
    ocamlc impparser.mli
    ocamlc impparser.ml
fi

if [[ -f "implexer.mll" ]]; then
    ocamllex implexer.mll
    ocamlc implexer.ml
fi
