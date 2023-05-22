pub const in = std.io.getStdIn().reader();
pub const out = std.io.getStdOut().writer();
pub const err = std.io.getStdErr().writer();

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
