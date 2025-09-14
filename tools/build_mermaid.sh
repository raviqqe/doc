#!/bin/sh

set -ex

for file in $(find . -name '*.mmd'); do
  mmdc \
    --puppeteerConfigFile puppeteer.json \
    --configFile mermaid.json \
    --theme neutral \
    --cssFile mermaid.css \
    -i $file -o ${file%.mmd}.svg
done
