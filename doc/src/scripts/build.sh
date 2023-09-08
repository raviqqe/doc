#!/bin/sh

set -e

directory=$(dirname $0)/..

npx @marp-team/marp-cli --html --input-dir $directory/../.. --output $directory
npx astro build
