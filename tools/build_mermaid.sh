#!/bin/sh

set -ex

for file in $(find . -name '*.mmd'); do
  mmdc \
    --backgroundColor transparent \
    --configFile mermaid.json \
    --cssFile mermaid.css \
    --puppeteerConfigFile puppeteer.json \
    --theme dark \
    -i $file -o ${file%.mmd}.svg
done
