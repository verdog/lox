fn run_bench_test(text: []const u8) !void {
    var times: [3]i64 = undefined;
    var heap = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer heap.deinit();

    for (&times, 0..) |*t, i| {
        _ = heap.reset(.free_all);
        var alctr = heap.allocator();

        const start_time = std.time.milliTimestamp();
        var vm: VM = undefined;

        vm.init_in_place(alctr);
        defer vm.deinit();

        var out_buf = std.ArrayList(u8).init(std.testing.allocator);
        defer out_buf.deinit();

        var err_buf = std.ArrayList(u8).init(std.testing.allocator);
        defer err_buf.deinit();

        var out = out_buf.writer();
        var err = err_buf.writer();
        var outs = .{ .out = out, .err = err };

        std.debug.print("{d}... ", .{i + 1});
        try vm.interpret(text, std.testing.allocator, outs);
        t.* = std.time.milliTimestamp() - start_time;
    }

    std.mem.sort(i64, &times, {}, std.sort.asc(i64));
    std.debug.print("\ntimes: {any}\nmedian: {d} milliseconds\n\n", .{ times, times[1] });
}

test "./benchmark/string_equality.lox" {
    try run_bench_test(@embedFile("./test/benchmark/string_equality.lox"));
}

test "./benchmark/zoo.lox" {
    try run_bench_test(@embedFile("./test/benchmark/zoo.lox"));
}

test "./benchmark/properties.lox" {
    try run_bench_test(@embedFile("./test/benchmark/properties.lox"));
}

test "./benchmark/invocation.lox" {
    try run_bench_test(@embedFile("./test/benchmark/invocation.lox"));
}

// zoo_batch is different: it gives itself ten seconds and measures how many operations it
// can do in that time. this doesn't fit our other benchmarks where a lower time is
// better, so I'm disabling it (for now?)
// test "./benchmark/zoo_batch.lox" {
//     try run_bench_test(@embedFile("./test/benchmark/zoo_batch.lox"));
// }

test "./benchmark/fib.lox" {
    try run_bench_test(@embedFile("./test/benchmark/fib.lox"));
}

test "./benchmark/trees.lox" {
    try run_bench_test(@embedFile("./test/benchmark/trees.lox"));
}

test "./benchmark/method_call.lox" {
    try run_bench_test(@embedFile("./test/benchmark/method_call.lox"));
}

test "./benchmark/binary_trees.lox" {
    try run_bench_test(@embedFile("./test/benchmark/binary_trees.lox"));
}

test "./benchmark/equality.lox" {
    try run_bench_test(@embedFile("./test/benchmark/equality.lox"));
}

test "./benchmark/instantiation.lox" {
    try run_bench_test(@embedFile("./test/benchmark/instantiation.lox"));
}

const std = @import("std");
const VM = @import("VM.zig");
