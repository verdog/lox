#!/bin/bash

if which poop
then
  for bench in ./src/test/benchmark/*.lox; do
    basename="$(basename $bench)"
    bin=$(zig test -Drelease=true --test-no-exec --test-filter /$basename --verbose-link ./src/test_benchmarks.zig |& cut -d ' ' -f13)
    echo "$bench - $basename - $bin"
    poop --duration 10000 "$bin"
  done
else
  zig test -Dreleas=true ./src/test_benchmarks.zig
fi
