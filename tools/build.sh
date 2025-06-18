#!/bin/sh

set -ex

root_directory=$PWD

directory=doc/public/papers
mkdir -p $directory

for file in $(find . -name '*.tex'); do
  (
    cd $(dirname $file)

    file=$(basename ${file%.tex})

    rm -f *.aux

    for mermaid_file in *.mmd; do
      npx --package @mermaid-js/mermaid-cli -- \
        mmdc ${CI:+-p $root_directory/puppeteer.json} -i $mermaid_file -o ${mermaid_file%.mmd}.png
    done

    lualatex --halt-on-error $file
    bibtex $file
    lualatex --halt-on-error $file
    lualatex --halt-on-error $file
  )

  file=${file%.tex}
  cp $file.pdf $directory/$(basename $(dirname $file)).pdf
done
