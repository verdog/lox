pub const std_options = struct {
    pub const log_level = if (@import("builtin").mode == .Debug) .debug else .info;
};

pub fn main() !void {
    defer if (!gpa.detectLeaks()) log.debug("no leaks", .{});
    defer ux.stdout_buffer.flush() catch {};

    if (std.os.argv.len > 2) {
        try ux.out.print("Usage: {s} <script>\n", .{std.os.argv[0]});
        log.err("invalid arguments {s}", .{std.os.argv});
        std.os.exit(64);
    } else if (std.os.argv.len == 2) {
        try runFile(std.mem.sliceTo(std.os.argv[1], '\x00'));
    } else {
        try runPrompt();
    }
}

fn runFile(path: []const u8) !void {
    const file = std.fs.cwd().openFile(path, .{}) catch |e| switch (e) {
        error.FileNotFound => {
            log.err("file not found: {s}", .{path});
            return e;
        },
        else => return e,
    };
    defer file.close();

    const bytes = file.readToEndAlloc(heap, 1024 * 1024 * 1024) catch |e| switch (e) {
        error.FileTooBig => {
            log.err("file too big: {s}, file size must be less than 1GB", .{path});
            return e;
        },
        else => return e,
    };
    defer heap.free(bytes);

    var vm = VM.init();
    defer vm.deinit(heap);

    // TODO convert these errors to zig errors
    const result = vm.interpret(bytes, heap, ux.out);
    // TODO remove this flush
    ux.stdout_buffer.flush() catch {};
    switch (result) {
        .compile_error => std.os.exit(65),
        .runtime_error => std.os.exit(70),
        .ok => std.os.exit(0),
    }
}

fn runPrompt() !void {
    var input_buffer = [_]u8{'\x00'} ** 1024;

    var vm = VM.init();
    defer vm.deinit(heap);

    while (true) {
        try ux.out.print("> ", .{});
        try ux.stdout_buffer.flush();
        const maybe_line = try ux.in.readUntilDelimiterOrEof(&input_buffer, '\n');
        if (maybe_line) |line| {
            // TODO convert these errors to zig errors
            const result = vm.interpret(line, heap, ux.out);
            // TODO remove this flush
            ux.stdout_buffer.flush() catch {};
            switch (result) {
                .compile_error => std.os.exit(65),
                .runtime_error => std.os.exit(70),
                .ok => {},
            }
        } else {
            break;
        }
    }
}

test "run all tests" {
    std.testing.refAllDeclsRecursive(@This());
}

const std = @import("std");
var gpa = std.heap.GeneralPurposeAllocator(.{
    .stack_trace_frames = 16,
}){};
var heap = gpa.allocator();

const log = std.log.scoped(.main);

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const VM = @import("VM.zig");

const ux = @import("ux.zig");
const dbg = @import("debug.zig");
