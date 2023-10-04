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

### Benchmark Data

These are times from my own machine and are really only meant for my own reference.

<details>
<summary>Expand</summary>

```
./src/test/benchmark/binary_trees.lox - binary_trees.lox
Benchmark 1 (27 runs): /home/josh/Code/lox/bytecode/zig-cache/o/73fcc1901016e54c999bbbd0700a12ea/test
  measurement          mean ± σ            min … max           outliers
  wall_time          2.25s  ± 50.8ms    2.15s  … 2.35s           0 ( 0%)        
  peak_rss           2.47MB ±  111KB    2.29MB … 2.62MB          0 ( 0%)        
  cpu_cycles         10.2G  ±  170M     10.0G  … 10.5G           0 ( 0%)        
  instructions       32.0G  ±  249      32.0G  … 32.0G           1 ( 4%)        
  cache_references   30.4K  ± 6.34K     19.9K  … 44.0K           0 ( 0%)        
  cache_misses       7.10K  ± 1.10K     5.29K  … 8.97K           0 ( 0%)        
  branch_misses      17.6M  ± 76.2K     17.4M  … 17.7M           0 ( 0%)        

./src/test/benchmark/equality.lox - equality.lox
Benchmark 1 (37 runs): /home/josh/Code/lox/bytecode/zig-cache/o/a3eeb0a2fe05deed7af82bc5ceeddfac/test
  measurement          mean ± σ            min … max           outliers
  wall_time          1.65s  ± 98.8ms    1.62s  … 2.23s           1 ( 3%)        
  peak_rss           1.99MB ± 82.0KB    1.97MB … 2.29MB          4 (11%)        
  cpu_cycles         7.31G  ±  477M     7.16G  … 10.1G           1 ( 3%)        
  instructions       32.4G  ± 1.61K     32.4G  … 32.4G           0 ( 0%)        
  cache_references   18.4K  ± 2.51K     14.2K  … 26.0K           2 ( 5%)        
  cache_misses       5.09K  ±  822      3.23K  … 7.22K           4 (11%)        
  branch_misses      6.92M  ± 66.5K     6.73M  … 7.03M           1 ( 3%)        

./src/test/benchmark/fib.lox - fib.lox
Benchmark 1 (10 runs): /home/josh/Code/lox/bytecode/zig-cache/o/c2e6094f55b2c28a866fc375ae978c0d/test
  measurement          mean ± σ            min … max           outliers
  wall_time          6.36s  ± 5.27ms    6.35s  … 6.37s           0 ( 0%)        
  peak_rss           2.03MB ± 84.6KB    1.97MB … 2.13MB          0 ( 0%)        
  cpu_cycles         28.3G  ± 11.4M     28.3G  … 28.3G           0 ( 0%)        
  instructions        105G  ±  104       105G  …  105G           2 (20%)        
  cache_references   19.8K  ± 4.40K     14.3K  … 28.6K           0 ( 0%)        
  cache_misses       5.73K  ± 1.48K     3.86K  … 8.05K           0 ( 0%)        
  branch_misses      29.9M  ± 28.3K     29.8M  … 29.9M           0 ( 0%)        

./src/test/benchmark/instantiation.lox - instantiation.lox
Benchmark 1 (27 runs): /home/josh/Code/lox/bytecode/zig-cache/o/e63ad1fa294fabd19bf26b487bc3c2af/test
  measurement          mean ± σ            min … max           outliers
  wall_time          2.23s  ± 6.43ms    2.22s  … 2.25s           1 ( 4%)        
  peak_rss           2.04MB ±  123KB    1.97MB … 2.29MB          0 ( 0%)        
  cpu_cycles         9.96G  ± 8.44M     9.94G  … 9.98G           0 ( 0%)        
  instructions       31.0G  ±  606      31.0G  … 31.0G           5 (19%)        
  cache_references   19.1K  ± 4.54K     13.5K  … 33.5K           2 ( 7%)        
  cache_misses       5.52K  ±  826      3.79K  … 6.90K           0 ( 0%)        
  branch_misses      1.73M  ± 27.1K     1.68M  … 1.79M           0 ( 0%)        

./src/test/benchmark/invocation.lox - invocation.lox
Benchmark 1 (33 runs): /home/josh/Code/lox/bytecode/zig-cache/o/924ad7dfe38a0fa6271baa9fb5b56939/test
  measurement          mean ± σ            min … max           outliers
  wall_time          1.83s  ± 21.3ms    1.81s  … 1.89s           0 ( 0%)        
  peak_rss           2.14MB ±  115KB    1.97MB … 2.29MB          0 ( 0%)        
  cpu_cycles         8.08G  ± 4.68M     8.07G  … 8.09G           0 ( 0%)        
  instructions       30.3G  ±  818      30.3G  … 30.3G           6 (18%)        
  cache_references   17.4K  ± 4.04K     13.3K  … 31.9K           3 ( 9%)        
  cache_misses       4.61K  ± 1.06K     2.89K  … 7.76K           1 ( 3%)        
  branch_misses       507K  ± 1.81K      505K  …  512K           0 ( 0%)        

./src/test/benchmark/method_call.lox - method_call.lox
Benchmark 1 (50 runs): /home/josh/Code/lox/bytecode/zig-cache/o/920540c2afca3860d80668f558e0f70e/test
  measurement          mean ± σ            min … max           outliers
  wall_time          1.21s  ± 5.07ms    1.20s  … 1.23s           1 ( 2%)        
  peak_rss           2.05MB ±  111KB    1.97MB … 2.29MB          0 ( 0%)        
  cpu_cycles         5.28G  ± 8.70M     5.27G  … 5.31G           1 ( 2%)        
  instructions       19.8G  ± 11.6K     19.8G  … 19.8G           7 (14%)        
  cache_references   20.2K  ± 3.87K     14.6K  … 36.2K           2 ( 4%)        
  cache_misses       6.22K  ± 1.31K     3.60K  … 8.97K           0 ( 0%)        
  branch_misses      1.79M  ±  102K     1.65M  … 2.08M           2 ( 4%)        

./src/test/benchmark/properties.lox - properties.lox
Benchmark 1 (22 runs): /home/josh/Code/lox/bytecode/zig-cache/o/966d3578c393187f9055b70848befc52/test
  measurement          mean ± σ            min … max           outliers
  wall_time          2.78s  ± 30.5ms    2.76s  … 2.91s           1 ( 5%)        
  peak_rss           2.13MB ± 87.6KB    1.97MB … 2.29MB          6 (27%)        
  cpu_cycles         12.2G  ±  135M     12.1G  … 12.8G           1 ( 5%)        
  instructions       44.1G  ± 1.20K     44.1G  … 44.1G           0 ( 0%)        
  cache_references   21.4K  ± 4.66K     14.3K  … 32.2K           1 ( 5%)        
  cache_misses       7.13K  ± 1.02K     4.98K  … 9.03K           0 ( 0%)        
  branch_misses      11.0M  ± 2.89K     11.0M  … 11.0M           0 ( 0%)        

./src/test/benchmark/string_equality.lox - string_equality.lox
Benchmark 1 (7 runs): /home/josh/Code/lox/bytecode/zig-cache/o/317d60974ce24d31a53e6640bfda1166/test
  measurement          mean ± σ            min … max           outliers
  wall_time          9.53s  ± 67.0ms    9.43s  … 9.64s           0 ( 0%)        
  peak_rss           2.20MB ±  129KB    2.13MB … 2.46MB          0 ( 0%)        
  cpu_cycles         41.9G  ±  262M     41.6G  … 42.3G           0 ( 0%)        
  instructions        154G  ± 14.4K      154G  …  154G           0 ( 0%)        
  cache_references   22.0K  ± 7.62K     15.3K  … 38.1K           0 ( 0%)        
  cache_misses       8.30K  ± 1.31K     6.58K  … 9.87K           0 ( 0%)        
  branch_misses      35.9M  ±  112K     35.7M  … 36.0M           0 ( 0%)        

./src/test/benchmark/trees.lox - trees.lox
Benchmark 1 (5 runs): /home/josh/Code/lox/bytecode/zig-cache/o/0ad13261a63398b8f3af7ba97cb9bd03/test
  measurement          mean ± σ            min … max           outliers
  wall_time          12.2s  ± 38.1ms    12.1s  … 12.2s           0 ( 0%)        
  peak_rss           7.11MB ±  147KB    7.05MB … 7.37MB          0 ( 0%)        
  cpu_cycles         53.4G  ± 99.0M     53.2G  … 53.5G           0 ( 0%)        
  instructions        178G  ± 3.69K      178G  …  178G           0 ( 0%)        
  cache_references   1.44G  ± 2.61M     1.44G  … 1.44G           0 ( 0%)        
  cache_misses        697K  ±  234K      495K  … 1.08M           0 ( 0%)        
  branch_misses      43.0M  ±  422K     42.3M  … 43.4M           1 (20%)        

./src/test/benchmark/zoo.lox - zoo.lox
Benchmark 1 (26 runs): /home/josh/Code/lox/bytecode/zig-cache/o/817fe6aa2118ba2116079a59d89ffb67/test
  measurement          mean ± σ            min … max           outliers
  wall_time          2.36s  ± 11.4ms    2.34s  … 2.39s           0 ( 0%)        
  peak_rss           2.03MB ±  104KB    1.97MB … 2.29MB          0 ( 0%)        
  cpu_cycles         10.3G  ± 27.7M     10.2G  … 10.3G           0 ( 0%)        
  instructions       36.9G  ±  762      36.9G  … 36.9G           0 ( 0%)        
  cache_references   19.4K  ± 4.31K     13.7K  … 30.3K           3 (12%)        
  cache_misses       6.55K  ± 1.32K     4.29K  … 9.02K           0 ( 0%)        
  branch_misses      10.6M  ± 45.1K     10.5M  … 10.7M           1 ( 4%)        
```
</details>
