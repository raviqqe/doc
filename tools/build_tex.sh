#!/bin/sh

set -ex

directory=doc/public/papers
mkdir -p $directory

for file in $(find . -name '*.tex'); do
  (
    cd $(dirname $file)

    file=$(basename ${file%.tex})

    rm -f *.aux

    texlive() {
      docker run --rm --mount type=bind,src=$PWD,dst=/workdir texlive/texlive "$@"
    }

    latex="lualatex --halt-on-error --shell-escape $file"

    texlive $latex
    texlive bibtex $file
    texlive $latex
    texlive $latex
  )

  file=${file%.tex}
  cp $file.pdf $directory/$(basename $(dirname $file)).pdf
done
