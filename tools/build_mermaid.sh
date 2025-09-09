#!/bin/sh

set -ex

for file in $(find . -name '*.mmd'); do
  pnpx mmdc -i $file -o ${file%.mmd}.svg
done
