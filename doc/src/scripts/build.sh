#!/bin/sh

set -ex

directory=$(dirname $0)/..

npx @marp-team/marp-cli --html --input-dir $directory/../.. --output $directory/pages

rm -r $directory/pages/doc
rm $directory/pages/README.html

npx astro build
