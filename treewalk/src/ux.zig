pub const in = std.io.getStdIn().reader();

const stdout_unbuffered = std.io.getStdOut().writer();
pub var stdout_buffer = std.io.bufferedWriter(stdout_unbuffered);
pub const out = stdout_buffer.writer();

const stderr_unbuffered = std.io.getStdErr();
pub const err = stderr_unbuffered.writer();

pub const Result = struct {
    had_error: bool = false,
};

pub fn printUserError(line: i32, text_to_blame: []const u8, message: []const u8) void {
    err.print("[line {}] Error: {s}: {s}\n", .{ line, text_to_blame, message }) catch |e| {
        log.err("couldn't print to stderr ({})", .{e});
    };
}

pub fn printUserErrorAtToken(token: lex.Token, message: []const u8) void {
    switch (token.typ) {
        .eof => printUserError(token.line, "eof", message),
        else => printUserError(token.line, token.lexeme, message),
    }
}

const std = @import("std");
const lex = @import("lex.zig");

const log = std.log.scoped(.ux);
