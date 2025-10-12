#!/bin/sh

set -ex

for file in $(find . -name '*.drawio.svg'); do
  target=${file%.drawio.svg}.svg

  cp $file $target
  svgo $target
done
