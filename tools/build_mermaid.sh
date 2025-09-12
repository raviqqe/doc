#!/bin/sh

set -ex

for file in $(find . -name '*.mmd'); do
  mmdc --puppeteerConfigFile puppeteer.json --theme forest -i $file -o ${file%.mmd}.svg
done
