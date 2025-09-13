#!/bin/sh

set -ex

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
done
