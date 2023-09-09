#!/bin/sh

set -ex

directory=$(dirname $0)/../..

npx @marp-team/marp-cli --html --input-dir $directory/.. --output $directory/public

rm -rf $directory/public/doc

npm run build:toc
npx astro build
