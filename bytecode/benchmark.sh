#!/bin/bash

set -e

if which poop
then
  if [ "$#" -eq 2 ]
  then
    for bench in ./src/test/benchmark/*.lox; do
      basename="$(basename $bench)"
      git checkout $1
      bin=$(zig test -Drelease=true --test-no-exec --test-filter /$basename --verbose-link ./src/test_benchmarks.zig |& cut -d ' ' -f13)
      cp $bin ./old_test

      git checkout $2
      bin=$(zig test -Drelease=true --test-no-exec --test-filter /$basename --verbose-link ./src/test_benchmarks.zig |& cut -d ' ' -f13)
      cp $bin ./new_test

      sha1sum ./old_test
      sha1sum ./new_test

      echo "$bench - $basename"
      poop --duration 10000 "./old_test" "./new_test"
    done
  else
    for bench in ./src/test/benchmark/*.lox; do
      basename="$(basename $bench)"
      bin=$(zig test -Drelease=true --test-no-exec --test-filter /$basename --verbose-link ./src/test_benchmarks.zig |& cut -d ' ' -f13)
      echo "$bench - $basename - $bin"
      poop --duration 10000 "$bin"
    done
  fi
else
  zig test -Dreleas=true ./src/test_benchmarks.zig
fi
