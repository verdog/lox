var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var heap = gpa.allocator();

const log = std.log.scoped(.main);
pub const std_options = struct {
    pub const log_level = if (@import("builtin").mode == .Debug) .debug else .info;
};

const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

pub fn main() !void {
    defer if (!gpa.detectLeaks()) log.debug("no leaks", .{});

    if (std.os.argv.len > 2) {
        try stdout.print("Usage: {s} <script>\n", .{std.os.argv[0]});
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
            stderr.print("File not found: {s}\n", .{path}) catch {};
            log.err("file not found: {s}", .{path});
            return e;
        },
        else => return e,
    };
    defer file.close();

    const bytes = file.readToEndAlloc(heap, 1024 * 1024 * 1024) catch |e| switch (e) {
        error.FileTooBig => {
            stderr.print("Only input files up to 1GB are supported\n", .{}) catch {};
            log.err("file too big: {s}", .{path});
            return e;
        },
        else => return e,
    };
    defer heap.free(bytes);

    _ = try run(bytes);
}

fn runPrompt() !void {
    var input_buffer = [_]u8{'\x00'} ** 512;

    while (true) {
        try stdout.print("> ", .{});
        const maybe_line = try stdin.readUntilDelimiterOrEof(&input_buffer, '\n');
        if (maybe_line) |line| {
            if (run(line)) |result| {
                if (result.had_error) {
                    reportUserErrorWhere(0, line, "User error");
                }
            } else |e| {
                log.info("failed to run line: {s}, {}", .{ line, e });
            }
        } else {
            break;
        }
    }
}

const RunResult = struct {
    had_error: bool = false,
};

fn run(bytes: []const u8) !RunResult {
    log.debug("run({s})", .{bytes});
    return RunResult{};
}

fn reportUserError(line: i32, message: []const u8) void {
    reportUserErrorWhere(line, "", message);
}

fn reportUserErrorWhere(line: i32, where: []const u8, message: []const u8) void {
    stderr.print("[line {}] Error: {s}: {s}\n", .{ line, where, message }) catch |e| {
        log.err("couldn't print to stderr ({})", .{e});
    };
}

test "run all tests" {
    std.testing.refAllDecls(@This());
}

const std = @import("std");
