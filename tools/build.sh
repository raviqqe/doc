#!/bin/sh

set -ex

directory=doc/public/papers
mkdir -p $directory

for file in $(find . -name '*.tex'); do
  (
    cd $(dirname $file)

    file=$(basename ${file%.tex})

    rm -f *.aux

    for file in *.mmd; do
      npx --package @mermaid-js/mermaid-cli -- mmdc -i $file -o ${file%.mmd}.svg
    done

    lualatex --halt-on-error $file
    bibtex $file
    lualatex --halt-on-error $file
    lualatex --halt-on-error $file
  )

  file=${file%.tex}
  cp $file.pdf $directory/$(basename $(dirname $file)).pdf
done
