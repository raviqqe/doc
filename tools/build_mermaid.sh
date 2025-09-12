#!/bin/sh

set -ex

for file in $(find . -name '*.mmd'); do
  mmdc \
    --puppeteerConfigFile puppeteer.json \
    --configFile mermaid.json \
    --cssFile mermaid.css \
    -i $file -o ${file%.mmd}.svg
done
