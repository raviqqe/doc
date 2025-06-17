#!/bin/sh

set -ex

mkdir -p doc/public/papers

for file in $(find . -name '*.tex'); do
  (
    cd $(dirname $file)

    file=$(basename ${file%.tex})

    rm -f *.aux

    lualatex --halt-on-error $file
    bibtex $file
    lualatex --halt-on-error $file
    lualatex --halt-on-error $file
  )

  file=${file%.tex}
  cp $file.pdf doc/public/papers/$(basename $(dirname $file)).pdf
done
