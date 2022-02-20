#!/bin/sh

set -e

if [ $# -ne 1 ]; then
  exit 1
fi

file=$1

marp --pdf "$file"
