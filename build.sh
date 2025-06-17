#!/bin/sh

set -e

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
done
