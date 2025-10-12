#!/bin/sh

set -ex

for file in $(find . -name '*.drawio.svg'); do
  cp $file ${file%.drawio.svg}.svg
done
