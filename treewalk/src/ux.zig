pub const in = std.io.getStdIn().reader();

const stdout_unbuffered = std.io.getStdOut().writer();
var stdout_buffered = std.io.bufferedWriter(stdout_unbuffered);
pub const out = stdout_buffered.writer();

const stderr_unbuffered = std.io.getStdErr();
pub const err = stderr_unbuffered.writer();

pub const Result = struct {
    had_error: bool = false,
};

pub fn printUserError(line: i32, message: []const u8) void {
    printUserErrorWhere(line, "", message);
}

pub fn printUserErrorWhere(line: i32, where: []const u8, message: []const u8) void {
    err.print("[line {}] Error: {s}: {s}\n", .{ line, where, message }) catch |e| {
        log.err("couldn't print to stderr ({})", .{e});
    };
}

const std = @import("std");

const log = std.log.scoped(.ux);
