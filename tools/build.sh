#!/bin/sh

set -ex

directory=doc/public/papers
mkdir -p $directory

for file in $(find . -name '*.tex'); do
  (
    cd $(dirname $file)

    file=$(basename ${file%.tex})

    rm -f *.aux

    latex() {
      lualatex --halt-on-error --shell-escape $file
    }

    latex
    bibtex $file
    latex
    latex
  )

  file=${file%.tex}
  cp $file.pdf $directory/$(basename $(dirname $file)).pdf
done
