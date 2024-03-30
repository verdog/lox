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
zig test ./src/main.zig # slightly fancier output
```

## Benchmark Tests

Run with:

```
zig test [-Drelease=true] ./src/test_benchmark.zig
```
