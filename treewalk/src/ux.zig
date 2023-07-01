pub const in = std.io.getStdIn().reader();

const stdout_unbuffered = std.io.getStdOut().writer();
pub var stdout_buffer = std.io.bufferedWriter(stdout_unbuffered);
pub const out = stdout_buffer.writer();

const stderr_unbuffered = std.io.getStdErr();
pub const err = stderr_unbuffered.writer();

pub const Result = struct {
    had_error: bool = false,
};

pub fn printUserError(line: i32, message: []const u8) void {
    printUserErrorWhere(line, "", message);
}

pub fn printUserErrorWhere(line: i32, where: []const u8, message: []const u8) void {
    err.print("[line {}] Error: at {s}: {s}\n", .{ line, where, message }) catch |e| {
        log.err("couldn't print to stderr ({})", .{e});
    };
}

pub fn tokenError(token: lex.Token, message: []const u8) void {
    switch (token.typ) {
        .eof => printUserErrorWhere(token.line, "eof", message),
        else => printUserErrorWhere(token.line, token.lexeme, message),
    }
}

const std = @import("std");
const lex = @import("lex.zig");

const log = std.log.scoped(.ux);
