#!/bin/sh

set -e

cd $(dirname $0)/../../..

mkdir -p doc/src/components

for file in $(find . -name '*.md' | sort | grep -iv -e node_modules -e readme -e doc); do
  path=$(echo $file | sed 's/\.md/\.html/g' | sed 's/^\.\///')
  title=$(cat $file | grep '^# .*$' | head -n 1 | sed 's/# *//g')

  echo "- [$title]($path)"
done >doc/src/components/TableOfContents.md
