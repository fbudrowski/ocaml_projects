#!/bin/bash

cd "$(dirname "$0")"

echo Compiling arithmetics

ocamlc -c arytmetyka.mli arytmetyka.ml

for f in tests/*
do
    echo Processing: $(basename "$f")
    ocamlc -c "$f"
    ocamlc -o "${f%%.*}" arytmetyka.cmo "${f%%.*}".cmo
    ./"${f%%.*}"
    rm "${f%%.*}" "${f%%.*}".cmo "${f%%.*}".cmi
done
