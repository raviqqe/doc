#!/bin/sh

set -ex

for file in $(find . -name '*.mmd'); do
  mmdc -i $file -o ${file%.mmd}.svg
done
