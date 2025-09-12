#!/bin/sh

set -ex

for file in $(find . -name '*.mmd'); do
  mmdc --puppeteerConfigFile puppeteer.json --width 1600 --height 900 --theme forest -i $file -o ${file%.mmd}.svg
done
