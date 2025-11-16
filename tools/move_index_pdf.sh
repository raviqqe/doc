#!/bin/sh

set -e

for path in $(find . -name index.pdf); do
  mv $path $(dirname $path).pdf
done
