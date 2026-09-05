#!/bin/sh

set -e

for file in $(find . -name '*.mmd'); do
  echo -i $file -o ${file%.mmd}.svg
done |
  xargs -L 1 -P 0 \
    mmdc \
    --backgroundColor transparent \
    --configFile mermaid.json \
    --cssFile mermaid.css \
    --puppeteerConfigFile puppeteer.json \
    --theme dark
