#!/bin/sh

set -e

if [ $# -ne 1 ]; then
  exit 1
fi

file=$1

kill_all() {
  echo $pids
  kill $pids
}

trap kill_all SIGTERM

marp -w "$file" &
pids=$1

(
  cd $(dirname "$file")
  python3 -m http.server
  pids="$pids $!"
)
