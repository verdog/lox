# Lox bytecode interpreter

This directory has a complete bytecode interpreter. It passes all tests from
https://github.com/munificent/craftinginterpreters/tree/master/test.

It's written in Zig 0.11.0.

## Build

```
zig build run
```

## Run a lox script:

(Lots of lox files in ./examples and in ./src/test)

```
zig build run -- ./path/to/lox/file
```

Run tests (Runs most scripts in ./src/test):

```
zig build test # silent output
zig test ./src/main.zig test # slightly fancier output
```

## Benchmark Tests

Run with:

```
zig test [-Drelease=true] ./src/test_benchmark.zig
```

### Times

These are times from my own machine and are really only meant for my own reference.

Generated with:
```
zig test -Drelease=true ./src/test_benchmark.zig
```

| Benchmark         | Initial completion of book (57287f5) |
| ----------------- | ------------------------------------ |
| `string_equality` | 9,1  seconds                         |
| `zoo`             | 2.3  seconds                         |
| `properties`      | 2.7  seconds                         |
| `invocation`      | 1.8  seconds                         |
| `fib`             | 6.4  seconds                         |
| `trees`           | 11.8 seconds                         |
| `method_call`     | 1.2  seconds                         |
| `binary_trees`    | 2.0  seconds                         |
| `equality`        | 1.7  seconds                         |
| `instantiation`   | 2.2  seconds                         |
