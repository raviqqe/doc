#!/bin/sh

set -e

cd $(dirname $0)/../../..

for file in $(find . -name '*.md' | grep -iv -e node_modules -e readme -e doc); do
  echo $file
  cat $file | grep '^# .*$' | head -n 1 | sed 's/# *//g'
done
