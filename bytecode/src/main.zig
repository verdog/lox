pub const std_options = struct {
    pub const log_level = if (@import("builtin").mode == .Debug) .debug else .info;
};

pub fn main() !u8 {
    defer if (!gpa.detectLeaks()) log.debug("no leaks", .{});
    defer ux.stdout_buffer.flush() catch {};

    if (std.os.argv.len > 2) {
        try ux.out.print("Usage: {s} <script>\n", .{std.os.argv[0]});
        log.err("invalid arguments {s}", .{std.os.argv});
        std.os.exit(64);
    } else if (std.os.argv.len == 2) {
        run_file(std.mem.sliceTo(std.os.argv[1], '\x00')) catch |e| switch (e) {
            VM.Error.compile_error => {
                try ux.out.print("Compile error\n", .{});
                return 65;
            },
            VM.Error.runtime_error => {
                try ux.out.print("Runtime error\n", .{});
                return 70;
            },
            else => return e,
        };
    } else {
        try run_prompt();
    }

    return 0;
}

fn run_file(path: []const u8) !void {
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

    dbg.Disassembler.border(path, ux.out);
    ux.out.print("{s}\n", .{bytes}) catch unreachable;

    var vm = VM.init(heap);
    defer vm.deinit();

    try vm.interpret(bytes, heap, ux.out);
}

fn run_prompt() !void {
    var input_buffer = [_]u8{'\x00'} ** 1024;

    var vm = VM.init(heap);
    defer vm.deinit();

    while (true) {
        try ux.out.print("> ", .{});
        try ux.stdout_buffer.flush();
        const maybe_line = try ux.in.readUntilDelimiterOrEof(&input_buffer, '\n');
        if (maybe_line) |line| {
            vm.interpret(line, heap, ux.out) catch |e| switch (e) {
                VM.Error.compile_error => try ux.out.print("Compile error\n", .{}),
                VM.Error.runtime_error => try ux.out.print("Runtime error\n", .{}),
            };
            ux.stdout_buffer.flush() catch {};
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
    .stack_trace_frames = 32,
}){};
var heap = gpa.allocator();

const log = std.log.scoped(.main);

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const VM = @import("VM.zig");

const ux = @import("ux.zig");
const dbg = @import("debug.zig");
