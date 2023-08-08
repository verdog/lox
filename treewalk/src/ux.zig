pub const in = std.io.getStdIn().reader();

pub const stdout_unbuffered = std.io.getStdOut().writer();
pub var stdout_buffer = std.io.bufferedWriter(stdout_unbuffered);
pub const out = stdout_buffer.writer();

const stderr_unbuffered = std.io.getStdErr();
pub const err = stderr_unbuffered.writer();

pub const Result = struct {
    had_error: bool = false,
};

pub fn printUserError(line: i32, text_to_blame: []const u8, comptime message: []const u8) void {
    if (message[message.len - 1] == '.')
        @compileError("Don't end error messages with '.'.");

    err.print("[line {}] Error: {s}: \"{s}\".\n", .{ line, message, text_to_blame }) catch |e| {
        log.err("couldn't print to stderr ({})", .{e});
    };
}

const std = @import("std");
const lex = @import("lex.zig");

const log = std.log.scoped(.ux);
