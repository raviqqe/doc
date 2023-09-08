#!/bin/sh

set -e

directory=$(dirname $0)/..

npx @marp-team/marp-cli --html --input-dir $directory/../.. --output $directory

rm -r $directory/doc
rm $directory/README.html

npx astro build
